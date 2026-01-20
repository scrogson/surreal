//! Generate .surreal binding files from Erlang and Elixir source files.
//!
//! Parses Erlang -spec/-type and Elixir @spec/@type declarations and converts them
//! to Dream extern mod syntax. Detects Result/Option patterns and generates
//! appropriate Dream types.

use std::collections::HashMap;
use std::fs;
use std::io::Write;
use std::path::Path;
use std::process::ExitCode;

/// Main entry point for the bindgen command.
pub fn cmd_bindgen(files: &[std::path::PathBuf], output: Option<&Path>, _module: Option<&str>) -> ExitCode {
    let mut all_modules: HashMap<String, ModuleInfo> = HashMap::new();

    for file in files {
        if !file.exists() {
            eprintln!("Error: file not found: {}", file.display());
            return ExitCode::from(1);
        }

        let source = match fs::read_to_string(file) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Error reading {}: {}", file.display(), e);
                return ExitCode::from(1);
            }
        };

        // Detect file type by extension
        let extension = file.extension().and_then(|s| s.to_str()).unwrap_or("");
        let is_elixir = extension == "ex" || extension == "exs";
        let is_header = extension == "hrl";

        if is_elixir {
            // Parse Elixir file - may contain multiple modules
            let type_defs = parse_elixir_type_defs(&source);
            let mut registry = TypeRegistry::new();
            for td in &type_defs {
                registry.register(td.clone());
            }

            // Parse all modules and their specs from this file
            let modules_with_specs = parse_elixir_modules_with_specs(&source, &registry);

            // Parse defstruct declarations for each module
            let all_structs = parse_elixir_structs_multi(&source);

            // Add each module's info separately
            for (module_name, specs) in modules_with_specs {
                let entry = all_modules
                    .entry(module_name.clone())
                    .or_insert_with(ModuleInfo::new);
                entry.specs.extend(specs);
                // Type defs go to all modules (shared within file)
                entry.type_defs.extend(type_defs.clone());
                // Add structs for this specific module
                if let Some(structs) = all_structs.get(&module_name) {
                    entry.structs.extend(structs.clone());
                }
            }

            continue; // Already added to all_modules, skip the common path below
        }

        let (module_name, type_defs, specs, records, structs) = if is_header {
            // Parse Erlang header file (.hrl)
            // Header files typically don't have a module declaration, use filename
            let module_name = file.file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("unknown")
                .to_string();

            let type_defs = parse_type_defs(&source);
            let records = parse_erlang_records(&source);

            // No specs in header files typically
            (module_name, type_defs, Vec::new(), records, Vec::new())
        } else {
            // Parse Erlang source file (.erl)
            let module_name = extract_module_name(&source)
                .unwrap_or_else(|| {
                    file.file_stem()
                        .and_then(|s| s.to_str())
                        .unwrap_or("unknown")
                        .to_string()
                });

            let type_defs = parse_type_defs(&source);
            let records = parse_erlang_records(&source);
            let mut registry = TypeRegistry::new();
            for td in &type_defs {
                registry.register(td.clone());
            }

            let specs = parse_specs(&source, &registry);
            (module_name, type_defs, specs, records, Vec::new())
        };

        let entry = all_modules
            .entry(module_name)
            .or_insert_with(ModuleInfo::new);
        entry.specs.extend(specs);
        entry.type_defs.extend(type_defs);
        entry.records.extend(records);
        entry.structs.extend(structs);
    }

    // Generate output
    let output_content = generate_surreal(&all_modules);

    // Write to file or stdout
    if let Some(output_path) = output {
        match fs::write(output_path, &output_content) {
            Ok(_) => {
                println!("Generated {}", output_path.display());
                ExitCode::SUCCESS
            }
            Err(e) => {
                eprintln!("Error writing {}: {}", output_path.display(), e);
                ExitCode::from(1)
            }
        }
    } else {
        let _ = std::io::stdout().write_all(output_content.as_bytes());
        ExitCode::SUCCESS
    }
}

/// Module information including specs, type definitions, records, and structs.
struct ModuleInfo {
    specs: Vec<ErlangSpec>,
    type_defs: Vec<ErlangTypeDef>,
    records: Vec<ErlangRecord>,
    structs: Vec<ElixirStruct>,
}

impl ModuleInfo {
    fn new() -> Self {
        Self {
            specs: Vec::new(),
            type_defs: Vec::new(),
            records: Vec::new(),
            structs: Vec::new(),
        }
    }
}

/// An Erlang type definition (-type or -opaque).
#[derive(Debug, Clone)]
struct ErlangTypeDef {
    name: String,
    #[allow(dead_code)]
    params: Vec<String>,
    definition: ErlangType,
}

/// An Erlang record definition (for structs).
#[derive(Debug, Clone)]
struct ErlangRecord {
    name: String,
    fields: Vec<(String, ErlangType)>,
}

/// An Elixir struct definition.
#[derive(Debug, Clone)]
struct ElixirStruct {
    /// The module that defines this struct (e.g., "Jason.Error")
    module: String,
    fields: Vec<(String, ErlangType)>,
}

/// Type registry for resolving type references.
struct TypeRegistry {
    types: HashMap<String, ErlangTypeDef>,
}

impl TypeRegistry {
    fn new() -> Self {
        Self { types: HashMap::new() }
    }

    fn register(&mut self, typedef: ErlangTypeDef) {
        self.types.insert(typedef.name.clone(), typedef);
    }

    fn resolve(&self, name: &str) -> Option<&ErlangType> {
        self.types.get(name).map(|td| &td.definition)
    }
}

/// An Erlang function spec.
#[derive(Debug, Clone)]
struct ErlangSpec {
    name: String,
    params: Vec<(String, ErlangType)>, // (name, type) pairs
    return_type: ErlangType,
}

/// Erlang type representation.
#[derive(Debug, Clone, PartialEq)]
enum ErlangType {
    /// atom(), integer(), etc.
    Named(String),
    /// Atom literal like 'ok' or 'error'
    AtomLiteral(String),
    /// list(T)
    List(Box<ErlangType>),
    /// {A, B, C}
    Tuple(Vec<ErlangType>),
    /// fun((A, B) -> C)
    Fun { params: Vec<ErlangType>, ret: Box<ErlangType> },
    /// Type variable like T, A, Number
    Var(String),
    /// Union type: A | B
    Union(Vec<ErlangType>),
    /// Result<T, E> - detected from {ok, T} | {error, E}
    Result(Box<ErlangType>, Box<ErlangType>),
    /// Option<T> - detected from T | undefined or {ok, T} | undefined
    Option(Box<ErlangType>),
    /// Remote type like module:type()
    Remote(String, String),
    /// Struct type %Module{field1: type1, field2: type2}
    Struct { module: String, fields: Vec<(String, ErlangType)> },
    /// any() or term()
    Any,
}

/// Extract module name from -module(name). declaration (Erlang).
fn extract_module_name(source: &str) -> Option<String> {
    for line in source.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("-module(") {
            let start = "-module(".len();
            if let Some(end) = trimmed.find(").") {
                return Some(trimmed[start..end].trim().to_string());
            }
            if let Some(end) = trimmed.find(')') {
                return Some(trimmed[start..end].trim().to_string());
            }
        }
    }
    None
}

/// Extract module name from defmodule declaration (Elixir).
/// Handles: defmodule Foo.Bar do, defmodule Foo.Bar.Baz do
#[allow(dead_code)] // Used by tests
fn extract_elixir_module_name(source: &str) -> Option<String> {
    for line in source.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("defmodule ") {
            // defmodule Foo.Bar do
            let rest = trimmed.strip_prefix("defmodule ")?.trim();
            // Find the end of module name (before 'do' or ',')
            let end = rest.find(" do")
                .or_else(|| rest.find(','))
                .unwrap_or(rest.len());
            let module_name = rest[..end].trim();
            // Return with Elixir. prefix for proper BEAM name
            return Some(format!("Elixir.{}", module_name));
        }
    }
    None
}

/// Parse all @type declarations from Elixir source.
fn parse_elixir_type_defs(source: &str) -> Vec<ErlangTypeDef> {
    let mut type_defs = Vec::new();
    let mut in_type = false;
    let mut type_text = String::new();

    for line in source.lines() {
        let trimmed = line.trim();

        // Skip comments
        if trimmed.starts_with('#') {
            continue;
        }

        // Look for @type or @typep or @opaque
        if trimmed.starts_with("@type ") || trimmed.starts_with("@typep ") || trimmed.starts_with("@opaque ") {
            in_type = true;
            type_text = trimmed.to_string();

            // Check if type is complete (no trailing operators suggesting continuation)
            if is_elixir_type_complete(&type_text) {
                if let Some(td) = parse_elixir_single_type_def(&type_text) {
                    type_defs.push(td);
                }
                in_type = false;
                type_text.clear();
            }
        } else if in_type {
            type_text.push(' ');
            type_text.push_str(trimmed);

            if is_elixir_type_complete(&type_text) {
                if let Some(td) = parse_elixir_single_type_def(&type_text) {
                    type_defs.push(td);
                }
                in_type = false;
                type_text.clear();
            }
        }
    }

    type_defs
}

/// Check if an Elixir type definition is complete.
fn is_elixir_type_complete(text: &str) -> bool {
    let text = text.trim();
    // A type is incomplete if it ends with | or ::
    !text.ends_with('|') && !text.ends_with("::")
}

/// Parse a single Elixir @type declaration.
fn parse_elixir_single_type_def(text: &str) -> Option<ErlangTypeDef> {
    // Format: @type name :: definition
    // Or: @type name(param) :: definition
    let text = text.trim_start_matches("@type")
        .trim_start_matches("@typep")
        .trim_start_matches("@opaque")
        .trim();

    // Find :: separator
    let def_pos = text.find("::")?;

    // Parse name and params
    let name_part = text[..def_pos].trim();
    let (name, params) = if let Some(paren_pos) = name_part.find('(') {
        let name = name_part[..paren_pos].trim().to_string();
        let params_end = name_part.rfind(')')?;
        let params_str = &name_part[paren_pos + 1..params_end];
        let params: Vec<String> = if params_str.trim().is_empty() {
            Vec::new()
        } else {
            split_top_level(params_str, ',')
                .into_iter()
                .map(|s| s.trim().to_string())
                .collect()
        };
        (name, params)
    } else {
        (name_part.to_string(), Vec::new())
    };

    // Parse definition (convert Elixir types to Erlang-compatible format)
    let def_str = text[def_pos + 2..].trim();
    let definition = parse_elixir_type(def_str);

    Some(ErlangTypeDef {
        name,
        params,
        definition,
    })
}

/// Parse all modules and their @spec declarations from Elixir source.
/// Returns a map of module_name -> specs, properly handling files with multiple modules.
fn parse_elixir_modules_with_specs(
    source: &str,
    registry: &TypeRegistry,
) -> HashMap<String, Vec<ErlangSpec>> {
    let mut modules: HashMap<String, Vec<ErlangSpec>> = HashMap::new();
    let mut current_module: Option<String> = None;
    let mut in_spec = false;
    let mut spec_text = String::new();

    for line in source.lines() {
        let trimmed = line.trim();

        // Skip comments
        if trimmed.starts_with('#') {
            continue;
        }

        // Track module declarations
        if trimmed.starts_with("defmodule ") {
            if let Some(name) = extract_module_name_from_line(trimmed) {
                current_module = Some(name.clone());
                modules.entry(name).or_insert_with(Vec::new);
            }
        }

        if trimmed.starts_with("@spec ") {
            in_spec = true;
            spec_text = trimmed.to_string();

            // Check if spec is complete
            if is_elixir_spec_complete(&spec_text) {
                if let Some(spec) = parse_elixir_single_spec(&spec_text, registry) {
                    if let Some(ref module) = current_module {
                        modules.entry(module.clone()).or_insert_with(Vec::new).push(spec);
                    }
                }
                in_spec = false;
                spec_text.clear();
            }
        } else if in_spec {
            spec_text.push(' ');
            spec_text.push_str(trimmed);

            if is_elixir_spec_complete(&spec_text) {
                if let Some(spec) = parse_elixir_single_spec(&spec_text, registry) {
                    if let Some(ref module) = current_module {
                        modules.entry(module.clone()).or_insert_with(Vec::new).push(spec);
                    }
                }
                in_spec = false;
                spec_text.clear();
            }
        }
    }

    modules
}

/// Extract module name from a defmodule line.
fn extract_module_name_from_line(line: &str) -> Option<String> {
    let trimmed = line.trim();
    if !trimmed.starts_with("defmodule ") {
        return None;
    }
    let rest = trimmed.strip_prefix("defmodule ")?.trim();
    let end = rest
        .find(" do")
        .or_else(|| rest.find(','))
        .unwrap_or(rest.len());
    let module_name = rest[..end].trim();
    // Normalize to Elixir. prefix
    if module_name.starts_with("Elixir.") {
        Some(module_name.to_string())
    } else {
        Some(format!("Elixir.{}", module_name))
    }
}

/// Parse all @spec declarations from Elixir source (legacy, for single-module files).
#[allow(dead_code)] // Used by tests
fn parse_elixir_specs(source: &str, registry: &TypeRegistry) -> Vec<ErlangSpec> {
    let mut specs = Vec::new();
    let mut in_spec = false;
    let mut spec_text = String::new();

    for line in source.lines() {
        let trimmed = line.trim();

        // Skip comments
        if trimmed.starts_with('#') {
            continue;
        }

        if trimmed.starts_with("@spec ") {
            in_spec = true;
            spec_text = trimmed.to_string();

            // Check if spec is complete
            if is_elixir_spec_complete(&spec_text) {
                if let Some(spec) = parse_elixir_single_spec(&spec_text, registry) {
                    specs.push(spec);
                }
                in_spec = false;
                spec_text.clear();
            }
        } else if in_spec {
            spec_text.push(' ');
            spec_text.push_str(trimmed);

            if is_elixir_spec_complete(&spec_text) {
                if let Some(spec) = parse_elixir_single_spec(&spec_text, registry) {
                    specs.push(spec);
                }
                in_spec = false;
                spec_text.clear();
            }
        }
    }

    specs
}

/// Check if an Elixir spec is complete.
fn is_elixir_spec_complete(text: &str) -> bool {
    let text = text.trim();
    // A spec is incomplete if it ends with | or ::
    !text.ends_with('|') && !text.ends_with("::")
}

/// Parse a single Elixir @spec declaration.
fn parse_elixir_single_spec(spec: &str, registry: &TypeRegistry) -> Option<ErlangSpec> {
    // Format: @spec function_name(type1, type2) :: return_type
    // Or: @spec function_name(arg1 :: type1, arg2 :: type2) :: return_type
    let spec = spec.trim_start_matches("@spec").trim();

    // Find function name
    let paren_pos = spec.find('(')?;
    let name = spec[..paren_pos].trim().to_string();

    // Skip operators and internal functions
    if name.starts_with('_') || name.is_empty() {
        return None;
    }

    // Find :: separator for return type (after closing paren)
    let close_paren = find_matching_paren(spec, paren_pos)?;
    let rest = &spec[close_paren + 1..];
    let arrow_pos = rest.find("::")?;
    let return_str = rest[arrow_pos + 2..].trim();

    // Parse parameters
    let params_str = &spec[paren_pos + 1..close_paren];
    let params = parse_elixir_param_list(params_str, registry);

    // Parse return type
    let mut return_type = parse_elixir_type(return_str);
    return_type = resolve_type_refs(&return_type, registry);
    return_type = detect_result_option_pattern(return_type);

    Some(ErlangSpec {
        name,
        params,
        return_type,
    })
}

/// Find the matching closing parenthesis.
fn find_matching_paren(s: &str, open_pos: usize) -> Option<usize> {
    let mut depth = 0;
    for (i, c) in s[open_pos..].char_indices() {
        match c {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    return Some(open_pos + i);
                }
            }
            _ => {}
        }
    }
    None
}

/// Parse Elixir parameter list.
fn parse_elixir_param_list(s: &str, registry: &TypeRegistry) -> Vec<(String, ErlangType)> {
    if s.trim().is_empty() {
        return Vec::new();
    }

    split_top_level(s, ',')
        .into_iter()
        .enumerate()
        .map(|(i, t)| {
            let t = t.trim();
            // Check for "name :: type" annotation
            if let Some(pos) = t.find("::") {
                let param_name = sanitize_param_name(t[..pos].trim());
                let type_str = t[pos + 2..].trim();
                let mut ty = parse_elixir_type(type_str);
                ty = resolve_type_refs(&ty, registry);
                (param_name, ty)
            } else {
                // Just a type
                let mut ty = parse_elixir_type(t);
                ty = resolve_type_refs(&ty, registry);
                (format!("arg{}", i), ty)
            }
        })
        .collect()
}

/// Parse an Elixir type expression.
fn parse_elixir_type(s: &str) -> ErlangType {
    let s = s.trim();

    if s.is_empty() {
        return ErlangType::Any;
    }

    // Handle Elixir-specific type syntax

    // Handle union types (A | B)
    let parts = split_top_level(s, '|');
    if parts.len() > 1 {
        let types: Vec<ErlangType> = parts.iter().map(|p| parse_elixir_type(p.trim())).collect();
        return detect_result_option_pattern(ErlangType::Union(types));
    }

    // Handle tuple {A, B, C}
    if s.starts_with('{') && s.ends_with('}') {
        let inner = &s[1..s.len()-1];
        let elements = parse_elixir_type_list(inner);
        return ErlangType::Tuple(elements);
    }

    // Handle list [T]
    if s.starts_with('[') && s.ends_with(']') {
        let inner = &s[1..s.len()-1];
        if inner.is_empty() {
            return ErlangType::List(Box::new(ErlangType::Any));
        }
        return ErlangType::List(Box::new(parse_elixir_type(inner)));
    }

    // Handle map %{} or map()
    if s.starts_with("%{") || s == "map()" || s == "map" {
        // Check if it has typed fields: %{key: type, ...}
        if s.starts_with("%{") && s.ends_with('}') {
            let inner = &s[2..s.len()-1].trim();
            if !inner.is_empty() && inner.contains("::") {
                let fields = parse_elixir_struct_fields(inner);
                if !fields.is_empty() {
                    return ErlangType::Struct {
                        module: String::new(), // Anonymous struct
                        fields,
                    };
                }
            }
        }
        return ErlangType::Named("map".to_string());
    }

    // Handle struct types %Module{} or %__MODULE__{}
    if s.starts_with('%') && s.contains('{') {
        return parse_elixir_struct_type(s);
    }

    // Handle remote types: Module.type() or Module.type
    if s.contains('.') && !s.starts_with(':') && !s.starts_with('"') {
        if let Some(dot_pos) = s.rfind('.') {
            let module = &s[..dot_pos];
            let rest = &s[dot_pos + 1..];
            let type_name = if let Some(paren_pos) = rest.find('(') {
                &rest[..paren_pos]
            } else {
                rest
            };

            // Handle common Elixir types
            match (module, type_name) {
                ("String", "t") => return ErlangType::Named("string".to_string()),
                ("Integer", "t") => return ErlangType::Named("int".to_string()),
                ("Atom", "t") => return ErlangType::Named("atom".to_string()),
                ("Exception", "t") => return ErlangType::Any,
                _ => return ErlangType::Remote(module.to_string(), type_name.to_string()),
            }
        }
    }

    // Handle atom literals :ok, :error, etc.
    if s.starts_with(':') {
        let atom_name = s.trim_start_matches(':');
        return ErlangType::AtomLiteral(atom_name.to_string());
    }

    // Handle parameterized types: name(args)
    if let Some(paren_pos) = s.find('(') {
        if s.ends_with(')') {
            let name = &s[..paren_pos];
            let args_str = &s[paren_pos + 1..s.len() - 1];

            match name {
                "list" => {
                    if args_str.is_empty() {
                        return ErlangType::List(Box::new(ErlangType::Any));
                    }
                    return ErlangType::List(Box::new(parse_elixir_type(args_str)));
                }
                "nonempty_list" => {
                    if args_str.is_empty() {
                        return ErlangType::List(Box::new(ErlangType::Any));
                    }
                    return ErlangType::List(Box::new(parse_elixir_type(args_str)));
                }
                // Handle basic types with () suffix
                "any" | "term" => return ErlangType::Any,
                "atom" => return ErlangType::Named("atom".to_string()),
                "boolean" => return ErlangType::Named("boolean".to_string()),
                "integer" | "non_neg_integer" | "pos_integer" | "neg_integer" | "number" => {
                    return ErlangType::Named("int".to_string());
                }
                "float" => return ErlangType::Named("float".to_string()),
                "binary" | "bitstring" => return ErlangType::Named("binary".to_string()),
                "pid" => return ErlangType::Named("pid".to_string()),
                "reference" => return ErlangType::Named("ref".to_string()),
                "map" => return ErlangType::Named("map".to_string()),
                "tuple" => return ErlangType::Any,
                "iodata" | "iolist" => return ErlangType::Any,
                "charlist" => return ErlangType::Named("string".to_string()),
                "keyword" => return ErlangType::List(Box::new(ErlangType::Any)),
                "no_return" => return ErlangType::Any,
                _ => {
                    return ErlangType::Named(name.to_string());
                }
            }
        }
    }

    // Handle simple Elixir types
    match s {
        "any" | "term" | "any()" | "term()" => ErlangType::Any,
        "atom" | "atom()" => ErlangType::Named("atom".to_string()),
        "boolean" | "boolean()" | "bool" => ErlangType::Named("boolean".to_string()),
        "true" => ErlangType::AtomLiteral("true".to_string()),
        "false" => ErlangType::AtomLiteral("false".to_string()),
        "nil" => ErlangType::AtomLiteral("nil".to_string()),
        "integer" | "integer()" | "non_neg_integer" | "non_neg_integer()" |
        "pos_integer" | "pos_integer()" | "neg_integer" | "neg_integer()" |
        "number" | "number()" => ErlangType::Named("int".to_string()),
        "float" | "float()" => ErlangType::Named("float".to_string()),
        "binary" | "binary()" | "bitstring" | "bitstring()" => ErlangType::Named("binary".to_string()),
        "pid" | "pid()" => ErlangType::Named("pid".to_string()),
        "reference" | "reference()" => ErlangType::Named("ref".to_string()),
        "map" | "map()" => ErlangType::Named("map".to_string()),
        "tuple" | "tuple()" => ErlangType::Any,
        "iodata" | "iodata()" | "iolist" | "iolist()" => ErlangType::Any,
        "charlist" | "charlist()" => ErlangType::Named("string".to_string()),
        "keyword" | "keyword()" => ErlangType::List(Box::new(ErlangType::Any)),
        "no_return" | "no_return()" => ErlangType::Any,
        // Elixir shorthand types
        "t" => ErlangType::Any, // Usually refers to the module's main type
        s if s.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) => {
            // Type variable
            ErlangType::Var(s.to_string())
        }
        _ => {
            // Unknown type name
            ErlangType::Named(s.to_string())
        }
    }
}

/// Parse a comma-separated list of Elixir types.
fn parse_elixir_type_list(s: &str) -> Vec<ErlangType> {
    if s.trim().is_empty() {
        return Vec::new();
    }

    split_top_level(s, ',')
        .into_iter()
        .map(|t| parse_elixir_type(t.trim()))
        .collect()
}

/// Parse an Elixir struct type: %Module{field: type, ...} or %__MODULE__{...}
fn parse_elixir_struct_type(s: &str) -> ErlangType {
    let s = s.trim();

    // Find the module name and fields part
    let brace_pos = match s.find('{') {
        Some(pos) => pos,
        None => return ErlangType::Named("map".to_string()),
    };

    let module_part = &s[1..brace_pos]; // Skip leading %
    let fields_part = &s[brace_pos..];

    // Handle __MODULE__ or actual module name
    let module = if module_part == "__MODULE__" {
        String::new() // Will be filled in from context
    } else {
        module_part.to_string()
    };

    // Parse fields from {field: type, ...}
    let fields = if fields_part.starts_with('{') && fields_part.ends_with('}') {
        let inner = &fields_part[1..fields_part.len()-1];
        if inner.is_empty() {
            Vec::new()
        } else {
            parse_elixir_struct_fields(inner)
        }
    } else {
        Vec::new()
    };

    ErlangType::Struct { module, fields }
}

/// Parse struct fields from "field1: type1, field2: type2" or "field1 :: type1, field2 :: type2"
fn parse_elixir_struct_fields(s: &str) -> Vec<(String, ErlangType)> {
    let mut fields = Vec::new();

    for field in split_top_level(s, ',') {
        let field = field.trim();
        if field.is_empty() {
            continue;
        }

        // Handle "field :: type" (Elixir type spec syntax)
        if let Some(pos) = field.find("::") {
            let name = field[..pos].trim().to_string();
            let type_str = field[pos + 2..].trim();
            let ty = parse_elixir_type(type_str);
            fields.push((name, ty));
        }
        // Handle "field: type" (keyword list style)
        else if let Some(pos) = field.find(':') {
            let name = field[..pos].trim().to_string();
            let type_str = field[pos + 1..].trim();
            let ty = parse_elixir_type(type_str);
            fields.push((name, ty));
        }
        // Handle "required(:field) => type" (map type spec)
        else if field.starts_with("required(") || field.starts_with("optional(") {
            if let Some(arrow_pos) = field.find("=>") {
                let key_part = &field[..arrow_pos];
                let type_str = field[arrow_pos + 2..].trim();

                // Extract field name from required(:field) or optional(:field)
                let name = key_part
                    .trim_start_matches("required(")
                    .trim_start_matches("optional(")
                    .trim_end_matches(')')
                    .trim()
                    .trim_start_matches(':')
                    .to_string();

                let ty = parse_elixir_type(type_str);
                fields.push((name, ty));
            }
        }
    }

    fields
}

/// Parse defstruct declarations from Elixir source.
/// Handles: defstruct [:field1, :field2] and defstruct field1: nil, field2: value
#[allow(dead_code)] // Used by tests
fn parse_elixir_structs(source: &str, module_name: &str) -> Vec<ElixirStruct> {
    let mut structs = Vec::new();
    let mut in_defstruct = false;
    let mut defstruct_text = String::new();

    for line in source.lines() {
        let trimmed = line.trim();

        // Skip comments
        if trimmed.starts_with('#') {
            continue;
        }

        if trimmed.starts_with("defstruct ") || trimmed == "defstruct" {
            in_defstruct = true;
            defstruct_text = trimmed.to_string();

            // Check if defstruct is complete (single line)
            if is_defstruct_complete(&defstruct_text) {
                if let Some(st) = parse_single_defstruct(&defstruct_text, module_name) {
                    structs.push(st);
                }
                in_defstruct = false;
                defstruct_text.clear();
            }
        } else if in_defstruct {
            defstruct_text.push(' ');
            defstruct_text.push_str(trimmed);

            if is_defstruct_complete(&defstruct_text) {
                if let Some(st) = parse_single_defstruct(&defstruct_text, module_name) {
                    structs.push(st);
                }
                in_defstruct = false;
                defstruct_text.clear();
            }
        }
    }

    structs
}

/// Parse defstruct declarations from Elixir source, tracking which module each belongs to.
/// Returns a map of module_name -> structs for files with multiple modules.
fn parse_elixir_structs_multi(source: &str) -> HashMap<String, Vec<ElixirStruct>> {
    let mut structs: HashMap<String, Vec<ElixirStruct>> = HashMap::new();
    let mut current_module: Option<String> = None;
    let mut in_defstruct = false;
    let mut defstruct_text = String::new();

    for line in source.lines() {
        let trimmed = line.trim();

        // Skip comments
        if trimmed.starts_with('#') {
            continue;
        }

        // Track module declarations
        if trimmed.starts_with("defmodule ") {
            if let Some(name) = extract_module_name_from_line(trimmed) {
                current_module = Some(name);
            }
        }

        if trimmed.starts_with("defstruct ") || trimmed == "defstruct" {
            in_defstruct = true;
            defstruct_text = trimmed.to_string();

            // Check if defstruct is complete (single line)
            if is_defstruct_complete(&defstruct_text) {
                if let Some(ref module) = current_module {
                    if let Some(st) = parse_single_defstruct(&defstruct_text, module) {
                        structs.entry(module.clone()).or_insert_with(Vec::new).push(st);
                    }
                }
                in_defstruct = false;
                defstruct_text.clear();
            }
        } else if in_defstruct {
            defstruct_text.push(' ');
            defstruct_text.push_str(trimmed);

            if is_defstruct_complete(&defstruct_text) {
                if let Some(ref module) = current_module {
                    if let Some(st) = parse_single_defstruct(&defstruct_text, module) {
                        structs.entry(module.clone()).or_insert_with(Vec::new).push(st);
                    }
                }
                in_defstruct = false;
                defstruct_text.clear();
            }
        }
    }

    structs
}

/// Check if a defstruct declaration is complete.
fn is_defstruct_complete(text: &str) -> bool {
    let text = text.trim();
    // Count brackets to ensure balanced
    let open_brackets = text.chars().filter(|&c| c == '[').count();
    let close_brackets = text.chars().filter(|&c| c == ']').count();

    // Simple heuristic: defstruct ends when brackets are balanced
    if open_brackets > 0 {
        open_brackets == close_brackets
    } else {
        // For keyword list style: defstruct foo: nil, bar: nil
        // Check if ends with something reasonable (not a comma)
        !text.ends_with(',')
    }
}

/// Parse a single defstruct declaration.
fn parse_single_defstruct(text: &str, module_name: &str) -> Option<ElixirStruct> {
    let text = text.trim_start_matches("defstruct").trim();

    if text.is_empty() {
        return Some(ElixirStruct {
            module: module_name.trim_start_matches("Elixir.").to_string(),
            fields: Vec::new(),
        });
    }

    let mut fields = Vec::new();

    // Handle list style: [:field1, :field2]
    if text.starts_with('[') {
        let inner = text.trim_start_matches('[').trim_end_matches(']');
        for field in split_top_level(inner, ',') {
            let field = field.trim();
            if field.is_empty() {
                continue;
            }

            // Field name starts with : for atoms
            let name = field.trim_start_matches(':').to_string();
            fields.push((name, ErlangType::Any));
        }
    } else {
        // Handle keyword style: field1: nil, field2: default
        for field in split_top_level(text, ',') {
            let field = field.trim();
            if field.is_empty() {
                continue;
            }

            if let Some(colon_pos) = field.find(':') {
                let name = field[..colon_pos].trim().to_string();
                // Type is unknown from defstruct alone; we'll try to match with @type t
                fields.push((name, ErlangType::Any));
            }
        }
    }

    Some(ElixirStruct {
        module: module_name.trim_start_matches("Elixir.").to_string(),
        fields,
    })
}

/// Parse all -type declarations from Erlang source.
fn parse_type_defs(source: &str) -> Vec<ErlangTypeDef> {
    let mut type_defs = Vec::new();
    let mut in_type = false;
    let mut type_text = String::new();

    for line in source.lines() {
        let trimmed = line.trim();

        // Skip comments
        if trimmed.starts_with('%') {
            continue;
        }

        if trimmed.starts_with("-type") || trimmed.starts_with("-opaque") {
            in_type = true;
            type_text = trimmed.to_string();

            if type_text.ends_with('.') {
                if let Some(td) = parse_single_type_def(&type_text) {
                    type_defs.push(td);
                }
                in_type = false;
                type_text.clear();
            }
        } else if in_type {
            type_text.push(' ');
            type_text.push_str(trimmed);

            if trimmed.ends_with('.') {
                if let Some(td) = parse_single_type_def(&type_text) {
                    type_defs.push(td);
                }
                in_type = false;
                type_text.clear();
            }
        }
    }

    type_defs
}

/// Parse a single -type declaration.
fn parse_single_type_def(text: &str) -> Option<ErlangTypeDef> {
    // Format: -type name() :: definition.
    // Or: -type name(Param) :: definition.
    let text = text.trim_start_matches("-type")
        .trim_start_matches("-opaque")
        .trim();
    let text = text.trim_end_matches('.');

    // Find name and params
    let paren_pos = text.find('(')?;
    let name = text[..paren_pos].trim().to_string();

    // Find :: separator
    let def_pos = text.find("::")?;

    // Parse params between ( and )
    let params_end = text[..def_pos].rfind(')')?;
    let params_str = &text[paren_pos + 1..params_end];
    let params: Vec<String> = if params_str.trim().is_empty() {
        Vec::new()
    } else {
        split_top_level(params_str, ',')
            .into_iter()
            .map(|s| s.trim().to_string())
            .collect()
    };

    // Parse definition
    let def_str = text[def_pos + 2..].trim();
    let definition = parse_type(def_str);

    Some(ErlangTypeDef {
        name,
        params,
        definition,
    })
}

/// Parse all -record declarations from Erlang source.
/// Format: -record(name, {field1, field2 :: type, field3 = default :: type}).
fn parse_erlang_records(source: &str) -> Vec<ErlangRecord> {
    let mut records = Vec::new();
    let mut in_record = false;
    let mut record_text = String::new();

    for line in source.lines() {
        let trimmed = line.trim();

        // Skip comments
        if trimmed.starts_with('%') {
            continue;
        }

        if trimmed.starts_with("-record(") {
            in_record = true;
            record_text = trimmed.to_string();

            if record_text.ends_with(").") {
                if let Some(rec) = parse_single_record(&record_text) {
                    records.push(rec);
                }
                in_record = false;
                record_text.clear();
            }
        } else if in_record {
            record_text.push(' ');
            record_text.push_str(trimmed);

            if trimmed.ends_with(").") {
                if let Some(rec) = parse_single_record(&record_text) {
                    records.push(rec);
                }
                in_record = false;
                record_text.clear();
            }
        }
    }

    records
}

/// Parse a single -record declaration.
fn parse_single_record(text: &str) -> Option<ErlangRecord> {
    // Format: -record(name, {field1, field2 :: type, field3 = default :: type}).
    let text = text.trim_start_matches("-record(").trim();
    let text = text.trim_end_matches(").");

    // Find the comma separating name from fields
    let comma_pos = text.find(',')?;
    let name = text[..comma_pos].trim().to_string();

    // Find the fields block { ... }
    let fields_start = text.find('{')?;
    let fields_end = text.rfind('}')?;
    let fields_str = &text[fields_start + 1..fields_end];

    let mut fields = Vec::new();
    for field in split_top_level(fields_str, ',') {
        let field = field.trim();
        if field.is_empty() {
            continue;
        }

        // Parse field: "name", "name :: type", "name = default", "name = default :: type"
        let (field_name, field_type) = parse_record_field(field);
        fields.push((field_name, field_type));
    }

    Some(ErlangRecord { name, fields })
}

/// Parse a single record field.
fn parse_record_field(field: &str) -> (String, ErlangType) {
    let field = field.trim();

    // Check for type annotation: "name :: type" or "name = default :: type"
    if let Some(type_pos) = field.find("::") {
        // Get field name (before = or ::)
        let name_part = &field[..type_pos];
        let name = if let Some(eq_pos) = name_part.find('=') {
            name_part[..eq_pos].trim().to_string()
        } else {
            name_part.trim().to_string()
        };

        let type_str = field[type_pos + 2..].trim();
        let ty = parse_type(type_str);
        (name, ty)
    } else if let Some(eq_pos) = field.find('=') {
        // Just "name = default" with no type
        let name = field[..eq_pos].trim().to_string();
        (name, ErlangType::Any)
    } else {
        // Just "name"
        (field.to_string(), ErlangType::Any)
    }
}

/// Parse all -spec declarations from Erlang source.
fn parse_specs(source: &str, registry: &TypeRegistry) -> Vec<ErlangSpec> {
    let mut specs = Vec::new();
    let mut in_spec = false;
    let mut spec_text = String::new();

    for line in source.lines() {
        let trimmed = line.trim();

        // Skip comments
        if trimmed.starts_with('%') {
            continue;
        }

        if trimmed.starts_with("-spec") {
            in_spec = true;
            spec_text = trimmed.to_string();

            if spec_text.ends_with('.') {
                if let Some(spec) = parse_single_spec(&spec_text, registry) {
                    specs.push(spec);
                }
                in_spec = false;
                spec_text.clear();
            }
        } else if in_spec {
            spec_text.push(' ');
            spec_text.push_str(trimmed);

            if trimmed.ends_with('.') {
                if let Some(spec) = parse_single_spec(&spec_text, registry) {
                    specs.push(spec);
                }
                in_spec = false;
                spec_text.clear();
            }
        }
    }

    specs
}

/// Parse a single -spec declaration.
fn parse_single_spec(spec: &str, registry: &TypeRegistry) -> Option<ErlangSpec> {
    let spec = spec.trim_start_matches("-spec").trim();
    let spec = spec.trim_end_matches('.');

    // Find function name
    let paren_pos = spec.find('(')?;
    let mut name = spec[..paren_pos].trim().to_string();

    // Remove module prefix if present
    if let Some(colon_pos) = name.find(':') {
        name = name[colon_pos + 1..].to_string();
    }

    // Skip operators and internal functions
    if name.starts_with('\'') || name.contains('\'') || name.starts_with('_') {
        return None;
    }

    // Split on 'when' to separate main spec from type constraints
    let (main_spec, when_clause) = if let Some(when_pos) = spec.find(" when ") {
        (&spec[paren_pos..when_pos], Some(&spec[when_pos + 6..]))
    } else {
        (&spec[paren_pos..], None)
    };

    // Parse type variable constraints
    let type_var_map = parse_when_clause(when_clause);

    // Find -> to split params from return type
    let arrow_pos = main_spec.rfind("->")?;
    let params_str = main_spec[1..arrow_pos].trim();
    // Strip only the outermost closing paren (not all of them)
    let params_str = params_str.strip_suffix(')').unwrap_or(params_str);
    let return_str = main_spec[arrow_pos + 2..].trim();

    // Parse parameters with names
    let params = parse_param_list(params_str, &type_var_map, registry);

    // Parse return type and apply pattern detection
    let mut return_type = parse_type(return_str);
    return_type = resolve_type_vars(&return_type, &type_var_map);
    return_type = resolve_type_refs(&return_type, registry);
    return_type = detect_result_option_pattern(return_type);

    Some(ErlangSpec {
        name,
        params,
        return_type,
    })
}

/// Parse parameter list with names.
fn parse_param_list(s: &str, type_var_map: &HashMap<String, ErlangType>, registry: &TypeRegistry) -> Vec<(String, ErlangType)> {
    if s.trim().is_empty() {
        return Vec::new();
    }

    split_top_level(s, ',')
        .into_iter()
        .enumerate()
        .map(|(i, t)| {
            let t = t.trim();
            // Check for "Name :: Type" annotation
            if let Some(pos) = t.find("::") {
                let param_name = t[..pos].trim();
                // Sanitize param name (removes " | undefined" etc.)
                let param_name = sanitize_param_name(param_name);
                let type_str = t[pos + 2..].trim();
                let mut ty = parse_type(type_str);
                ty = resolve_type_vars(&ty, type_var_map);
                ty = resolve_type_refs(&ty, registry);
                (param_name, ty)
            } else {
                // Just a type variable or type name
                let param_name = if t.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                    // Type variable - use lowercase version as name
                    sanitize_param_name(&t.to_lowercase())
                } else {
                    format!("arg{}", i)
                };
                let mut ty = parse_type(t);
                ty = resolve_type_vars(&ty, type_var_map);
                ty = resolve_type_refs(&ty, registry);
                (param_name, ty)
            }
        })
        .collect()
}

/// Sanitize a parameter name to be a valid Dream identifier.
/// Handles Erlang union types in param names like "Req | undefined".
fn sanitize_param_name(name: &str) -> String {
    // Strip union type suffixes (e.g., "Req | undefined" -> "Req")
    let name = if let Some(pos) = name.find('|') {
        name[..pos].trim()
    } else {
        name
    };

    // Convert to lowercase for Dream convention
    let name = name.to_lowercase();

    // Replace any invalid characters with underscores
    let name: String = name
        .chars()
        .map(|c| if c.is_alphanumeric() || c == '_' { c } else { '_' })
        .collect();

    // Ensure it doesn't start with a digit
    if name.chars().next().map(|c| c.is_ascii_digit()).unwrap_or(true) {
        format!("arg_{}", name)
    } else {
        name
    }
}

/// Parse type constraints from 'when' clause.
fn parse_when_clause(when_clause: Option<&str>) -> HashMap<String, ErlangType> {
    let mut map = HashMap::new();

    if let Some(clause) = when_clause {
        for constraint in split_top_level(clause, ',') {
            let constraint = constraint.trim();
            if let Some(pos) = constraint.find("::") {
                let var = constraint[..pos].trim().to_string();
                let ty = parse_type(constraint[pos + 2..].trim());
                map.insert(var, ty);
            }
        }
    }

    map
}

/// Resolve type variable references using the when clause map.
fn resolve_type_vars(ty: &ErlangType, type_var_map: &HashMap<String, ErlangType>) -> ErlangType {
    let mut visited = std::collections::HashSet::new();
    resolve_type_vars_inner(ty, type_var_map, &mut visited)
}

fn resolve_type_vars_inner(
    ty: &ErlangType,
    type_var_map: &HashMap<String, ErlangType>,
    visited: &mut std::collections::HashSet<String>,
) -> ErlangType {
    match ty {
        ErlangType::Var(name) => {
            // Prevent infinite recursion on recursive type definitions like DeepList :: [term() | DeepList]
            if visited.contains(name) {
                return ErlangType::Any;
            }
            if let Some(resolved) = type_var_map.get(name) {
                visited.insert(name.clone());
                let result = resolve_type_vars_inner(resolved, type_var_map, visited);
                visited.remove(name);
                result
            } else {
                ErlangType::Any
            }
        }
        ErlangType::List(inner) => {
            ErlangType::List(Box::new(resolve_type_vars_inner(inner, type_var_map, visited)))
        }
        ErlangType::Tuple(elements) => {
            ErlangType::Tuple(elements.iter().map(|e| resolve_type_vars_inner(e, type_var_map, visited)).collect())
        }
        ErlangType::Fun { params, ret } => {
            ErlangType::Fun {
                params: params.iter().map(|p| resolve_type_vars_inner(p, type_var_map, visited)).collect(),
                ret: Box::new(resolve_type_vars_inner(ret, type_var_map, visited)),
            }
        }
        ErlangType::Union(types) => {
            ErlangType::Union(types.iter().map(|t| resolve_type_vars_inner(t, type_var_map, visited)).collect())
        }
        ErlangType::Struct { module, fields } => {
            ErlangType::Struct {
                module: module.clone(),
                fields: fields.iter()
                    .map(|(n, t)| (n.clone(), resolve_type_vars_inner(t, type_var_map, visited)))
                    .collect(),
            }
        }
        _ => ty.clone(),
    }
}

/// Resolve type references using the type registry.
fn resolve_type_refs(ty: &ErlangType, registry: &TypeRegistry) -> ErlangType {
    let mut visited = std::collections::HashSet::new();
    resolve_type_refs_inner(ty, registry, &mut visited)
}

fn resolve_type_refs_inner(
    ty: &ErlangType,
    registry: &TypeRegistry,
    visited: &mut std::collections::HashSet<String>,
) -> ErlangType {
    match ty {
        ErlangType::Named(name) => {
            // Prevent infinite recursion on cyclic type definitions
            if visited.contains(name) {
                return ty.clone();
            }
            if let Some(resolved) = registry.resolve(name) {
                visited.insert(name.clone());
                let result = resolve_type_refs_inner(resolved, registry, visited);
                visited.remove(name);
                result
            } else {
                ty.clone()
            }
        }
        ErlangType::Remote(module, name) => {
            resolve_remote_type(module, name)
        }
        ErlangType::List(inner) => {
            ErlangType::List(Box::new(resolve_type_refs_inner(inner, registry, visited)))
        }
        ErlangType::Tuple(elements) => {
            ErlangType::Tuple(elements.iter().map(|e| resolve_type_refs_inner(e, registry, visited)).collect())
        }
        ErlangType::Fun { params, ret } => {
            ErlangType::Fun {
                params: params.iter().map(|p| resolve_type_refs_inner(p, registry, visited)).collect(),
                ret: Box::new(resolve_type_refs_inner(ret, registry, visited)),
            }
        }
        ErlangType::Union(types) => {
            ErlangType::Union(types.iter().map(|t| resolve_type_refs_inner(t, registry, visited)).collect())
        }
        ErlangType::Struct { module, fields } => {
            ErlangType::Struct {
                module: module.clone(),
                fields: fields.iter()
                    .map(|(n, t)| (n.clone(), resolve_type_refs_inner(t, registry, visited)))
                    .collect(),
            }
        }
        _ => ty.clone(),
    }
}

/// Resolve known remote types.
fn resolve_remote_type(module: &str, name: &str) -> ErlangType {
    match (module, name) {
        // calendar module
        ("calendar", "datetime") => ErlangType::Tuple(vec![
            ErlangType::Tuple(vec![
                ErlangType::Named("int".into()),
                ErlangType::Named("int".into()),
                ErlangType::Named("int".into()),
            ]),
            ErlangType::Tuple(vec![
                ErlangType::Named("int".into()),
                ErlangType::Named("int".into()),
                ErlangType::Named("int".into()),
            ]),
        ]),
        ("calendar", "date") => ErlangType::Tuple(vec![
            ErlangType::Named("int".into()),
            ErlangType::Named("int".into()),
            ErlangType::Named("int".into()),
        ]),
        ("calendar", "time") => ErlangType::Tuple(vec![
            ErlangType::Named("int".into()),
            ErlangType::Named("int".into()),
            ErlangType::Named("int".into()),
        ]),

        // file module
        ("file", "filename") | ("file", "name") | ("file", "name_all") | ("file", "filename_all") => {
            ErlangType::Named("string".into())
        }
        ("file", "posix") => ErlangType::Named("atom".into()),
        ("file", "io_device") => ErlangType::Named("any".into()),

        // io module
        ("io", "device") => ErlangType::Named("any".into()),

        // unicode
        ("unicode", "chardata") | ("unicode", "charlist") => ErlangType::Any,

        // erlang
        ("erlang", "timestamp") => ErlangType::Tuple(vec![
            ErlangType::Named("int".into()),
            ErlangType::Named("int".into()),
            ErlangType::Named("int".into()),
        ]),

        _ => ErlangType::Any,
    }
}

/// Detect Result/Option patterns in union types.
fn detect_result_option_pattern(ty: ErlangType) -> ErlangType {
    match ty {
        ErlangType::Union(types) => {
            // Try to detect true | false -> boolean
            if detect_boolean_pattern(&types) {
                return ErlangType::Named("boolean".to_string());
            }

            // Try to detect {ok, T} | {error, E} -> Result<T, E>
            if let Some((ok_type, err_type)) = detect_ok_error_pattern(&types) {
                return ErlangType::Result(Box::new(ok_type), Box::new(err_type));
            }

            // Try to detect ok | {error, E} -> Result<(), E>
            if let Some(err_type) = detect_ok_atom_error_pattern(&types) {
                return ErlangType::Result(
                    Box::new(ErlangType::Tuple(vec![])),
                    Box::new(err_type),
                );
            }

            // Try to detect T | undefined -> Option<T>
            if let Some(inner_type) = detect_undefined_pattern(&types) {
                return ErlangType::Option(Box::new(inner_type));
            }

            // No pattern detected, keep as union
            ErlangType::Union(types)
        }
        _ => ty,
    }
}

/// Detect true | false pattern (boolean type).
fn detect_boolean_pattern(types: &[ErlangType]) -> bool {
    if types.len() != 2 {
        return false;
    }

    let mut has_true = false;
    let mut has_false = false;

    for ty in types {
        match ty {
            ErlangType::AtomLiteral(name) if name == "true" => has_true = true,
            ErlangType::AtomLiteral(name) if name == "false" => has_false = true,
            _ => {}
        }
    }

    has_true && has_false
}

/// Detect {ok, T} | {error, E} pattern.
fn detect_ok_error_pattern(types: &[ErlangType]) -> Option<(ErlangType, ErlangType)> {
    if types.len() != 2 {
        return None;
    }

    let mut ok_type = None;
    let mut err_type = None;

    for ty in types {
        if let ErlangType::Tuple(elements) = ty {
            if elements.len() == 2 {
                if is_ok_atom(&elements[0]) {
                    ok_type = Some(elements[1].clone());
                } else if is_error_atom(&elements[0]) {
                    err_type = Some(elements[1].clone());
                }
            }
        }
    }

    match (ok_type, err_type) {
        (Some(ok), Some(err)) => Some((ok, err)),
        _ => None,
    }
}

/// Detect ok | {error, E} pattern.
fn detect_ok_atom_error_pattern(types: &[ErlangType]) -> Option<ErlangType> {
    if types.len() != 2 {
        return None;
    }

    let mut has_ok_atom = false;
    let mut err_type = None;

    for ty in types {
        match ty {
            ErlangType::AtomLiteral(name) | ErlangType::Named(name) if name == "ok" || name == "atom" => {
                // Bare 'ok' atom
                has_ok_atom = true;
            }
            ErlangType::Tuple(elements) if elements.len() == 2 && is_error_atom(&elements[0]) => {
                err_type = Some(elements[1].clone());
            }
            _ => {}
        }
    }

    if has_ok_atom && err_type.is_some() {
        err_type
    } else {
        None
    }
}

/// Detect T | undefined pattern.
fn detect_undefined_pattern(types: &[ErlangType]) -> Option<ErlangType> {
    if types.len() != 2 {
        return None;
    }

    let mut undefined_found = false;
    let mut other_type = None;

    for ty in types {
        match ty {
            ErlangType::AtomLiteral(name) | ErlangType::Named(name)
                if name == "undefined" || name == "nil" => {
                undefined_found = true;
            }
            _ => {
                other_type = Some(ty.clone());
            }
        }
    }

    if undefined_found {
        other_type
    } else {
        None
    }
}

fn is_ok_atom(ty: &ErlangType) -> bool {
    matches!(ty, ErlangType::AtomLiteral(n) | ErlangType::Named(n) if n == "ok" || n == "atom")
}

fn is_error_atom(ty: &ErlangType) -> bool {
    matches!(ty, ErlangType::AtomLiteral(n) | ErlangType::Named(n) if n == "error" || n == "atom")
}

/// Split string on delimiter, respecting parentheses nesting.
fn split_top_level(s: &str, delim: char) -> Vec<&str> {
    let mut result = Vec::new();
    let mut depth: usize = 0;
    let mut start = 0;

    for (i, c) in s.char_indices() {
        match c {
            '(' | '[' | '{' | '<' => depth += 1,
            ')' | ']' | '}' | '>' => depth = depth.saturating_sub(1),
            c if c == delim && depth == 0 => {
                result.push(&s[start..i]);
                start = i + 1;
            }
            _ => {}
        }
    }

    if start < s.len() {
        result.push(&s[start..]);
    }

    result
}

/// Parse a single Erlang type.
fn parse_type(s: &str) -> ErlangType {
    let s = s.trim();

    // Empty type
    if s.is_empty() {
        return ErlangType::Any;
    }

    // Check for atom literals BEFORE stripping quotes
    if s.starts_with('\'') && s.ends_with('\'') && s.len() > 2 {
        let atom_name = &s[1..s.len()-1];
        return ErlangType::AtomLiteral(atom_name.to_string());
    }

    // Handle inline annotations: "Name :: Type"
    if let Some(pos) = s.find("::") {
        return parse_type(s[pos + 2..].trim());
    }

    // Handle Erlang map type: #{key := type, ...} or #{key => type, ...}
    if s.starts_with("#{") && s.ends_with('}') {
        return ErlangType::Named("map".to_string());
    }

    // Skip problematic types (but not maps which start with #{)
    if s.contains('?') || (s.starts_with('#') && !s.starts_with("#{")) {
        return ErlangType::Any;
    }

    // Handle union types (A | B)
    let parts = split_top_level(s, '|');
    if parts.len() > 1 {
        let types: Vec<ErlangType> = parts.iter().map(|p| parse_type(p.trim())).collect();
        return detect_result_option_pattern(ErlangType::Union(types));
    }

    // Handle tuple {A, B, C}
    if s.starts_with('{') && s.ends_with('}') {
        let inner = &s[1..s.len()-1];
        let elements = parse_type_list(inner);
        return ErlangType::Tuple(elements);
    }

    // Handle list [T]
    if s.starts_with('[') && s.ends_with(']') {
        let inner = &s[1..s.len()-1];
        if inner.is_empty() {
            return ErlangType::List(Box::new(ErlangType::Any));
        }
        return ErlangType::List(Box::new(parse_type(inner)));
    }

    // Handle remote types: module:type() or module:type
    if s.contains(':') && !s.starts_with(':') {
        if let Some(colon_pos) = s.find(':') {
            let module = &s[..colon_pos];
            let rest = &s[colon_pos + 1..];
            let type_name = if let Some(paren_pos) = rest.find('(') {
                &rest[..paren_pos]
            } else {
                rest
            };
            return ErlangType::Remote(module.to_string(), type_name.to_string());
        }
    }

    // Handle parameterized types: name(args)
    if let Some(paren_pos) = s.find('(') {
        if s.ends_with(')') {
            let name = &s[..paren_pos];
            let args_str = &s[paren_pos + 1..s.len() - 1];

            match name {
                "list" | "nonempty_list" => {
                    if args_str.is_empty() {
                        return ErlangType::List(Box::new(ErlangType::Any));
                    }
                    return ErlangType::List(Box::new(parse_type(args_str)));
                }
                "fun" => {
                    return parse_fun_type(args_str);
                }
                "function" => {
                    return ErlangType::Fun {
                        params: vec![],
                        ret: Box::new(ErlangType::Any),
                    };
                }
                "maybe_improper_list" | "nonempty_maybe_improper_list" | "nonempty_improper_list" => {
                    return ErlangType::List(Box::new(ErlangType::Any));
                }
                // Handle basic types with () suffix
                "integer" | "non_neg_integer" | "pos_integer" | "neg_integer" | "number" => {
                    return ErlangType::Named("int".to_string());
                }
                "atom" | "module" => {
                    return ErlangType::Named("atom".to_string());
                }
                "boolean" => {
                    return ErlangType::Named("boolean".to_string());
                }
                "binary" | "bitstring" => {
                    return ErlangType::Named("binary".to_string());
                }
                "float" => {
                    return ErlangType::Named("float".to_string());
                }
                "string" => {
                    return ErlangType::Named("string".to_string());
                }
                "pid" => {
                    return ErlangType::Named("pid".to_string());
                }
                "reference" | "ref" => {
                    return ErlangType::Named("ref".to_string());
                }
                "map" => {
                    return ErlangType::Named("map".to_string());
                }
                "any" | "term" => {
                    return ErlangType::Any;
                }
                "tuple" | "iodata" | "iolist" | "no_return" | "none" => {
                    return ErlangType::Any;
                }
                _ => {
                    // Other parameterized types
                    return ErlangType::Named(name.to_string());
                }
            }
        }
    }

    // Handle simple types
    match s {
        "any" | "term" | "_" | "dynamic" => ErlangType::Any,
        "atom" | "module" => ErlangType::Named("atom".to_string()),
        "boolean" => ErlangType::Named("boolean".to_string()),
        "true" => ErlangType::AtomLiteral("true".to_string()),
        "false" => ErlangType::AtomLiteral("false".to_string()),
        "ok" => ErlangType::AtomLiteral("ok".to_string()),
        "error" => ErlangType::AtomLiteral("error".to_string()),
        "undefined" => ErlangType::AtomLiteral("undefined".to_string()),
        "nil" => ErlangType::AtomLiteral("nil".to_string()),
        "integer" | "non_neg_integer" | "pos_integer" | "neg_integer" | "number" | "arity" => {
            ErlangType::Named("int".to_string())
        }
        "float" => ErlangType::Named("float".to_string()),
        "binary" | "bitstring" | "nonempty_binary" | "nonempty_bitstring" => {
            ErlangType::Named("binary".to_string())
        }
        "string" | "nonempty_string" => ErlangType::Named("string".to_string()),
        "pid" => ErlangType::Named("pid".to_string()),
        "reference" | "ref" => ErlangType::Named("ref".to_string()),
        "port" => ErlangType::Any,
        "node" => ErlangType::Named("atom".to_string()),
        "timeout" | "infinity" => ErlangType::Named("int".to_string()),
        "mfa" => ErlangType::Tuple(vec![
            ErlangType::Named("atom".to_string()),
            ErlangType::Named("atom".to_string()),
            ErlangType::Named("int".to_string()),
        ]),
        "iodata" | "iolist" => ErlangType::Any,
        "char" | "byte" => ErlangType::Named("int".to_string()),
        "[]" => ErlangType::List(Box::new(ErlangType::Any)),
        "map" => ErlangType::Named("map".to_string()),
        "tuple" => ErlangType::Any,
        "no_return" | "none" => ErlangType::Any,
        "identifier" => ErlangType::Any,
        s if s.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) => {
            ErlangType::Var(s.to_string())
        }
        _ => {
            // Unknown type name - might be a user-defined type
            ErlangType::Named(s.to_string())
        }
    }
}

/// Parse a comma-separated list of types.
fn parse_type_list(s: &str) -> Vec<ErlangType> {
    if s.trim().is_empty() {
        return Vec::new();
    }

    split_top_level(s, ',')
        .into_iter()
        .map(|t| parse_type(t.trim()))
        .collect()
}

/// Parse fun((A, B) -> C) type.
fn parse_fun_type(s: &str) -> ErlangType {
    let s = s.trim();

    if s.starts_with('(') {
        if let Some(arrow_pos) = s.find("->") {
            let mut depth = 0;
            let mut params_end = 0;
            for (i, c) in s.char_indices() {
                match c {
                    '(' => depth += 1,
                    ')' => {
                        depth -= 1;
                        if depth == 0 {
                            params_end = i;
                            break;
                        }
                    }
                    _ => {}
                }
            }

            let params_str = &s[1..params_end];
            let ret_str = &s[arrow_pos + 2..];

            let params = parse_type_list(params_str);
            let ret = parse_type(ret_str);

            return ErlangType::Fun {
                params,
                ret: Box::new(ret),
            };
        }
    }

    ErlangType::Fun {
        params: vec![],
        ret: Box::new(ErlangType::Any),
    }
}

/// Check if a type name is a known Dream type.
fn is_known_surreal_type(name: &str) -> bool {
    matches!(name,
        "int" | "float" | "string" | "atom" | "bool" | "pid" | "ref" |
        "binary" | "map" | "any"
    )
}

/// Check if a name conflicts with Dream keywords.
fn is_surreal_keyword(name: &str) -> bool {
    matches!(name,
        "binary" | "fn" | "let" | "mut" | "if" | "else" | "match" | "struct" |
        "enum" | "mod" | "pub" | "self" | "spawn" | "receive" | "after" |
        "return" | "use" | "as" | "impl" | "trait" | "for" | "when" | "true" |
        "false" | "extern" | "type" | "string" | "int" | "bool" | "float" |
        "integer" | "atom" | "pid" | "ref" | "map" | "list" | "tuple" | "bytes"
    )
}

/// Convert CamelCase to snake_case.
/// Examples: "EncodeError" -> "encode_error", "JSONParser" -> "json_parser"
fn camel_to_snake(s: &str) -> String {
    let mut result = String::new();
    for (i, c) in s.chars().enumerate() {
        if c.is_uppercase() {
            // Add underscore before uppercase (except at start)
            if i > 0 {
                result.push('_');
            }
            result.push(c.to_ascii_lowercase());
        } else {
            result.push(c);
        }
    }
    result
}

/// Sanitize a module name to be a valid Dream identifier.
/// Handles Elixir modules (Elixir.Foo.Bar -> foo_bar) and other special cases.
fn sanitize_module_name(name: &str) -> String {
    // Handle Elixir modules: Elixir.Foo.BarBaz -> foo_bar_baz
    if name.starts_with("Elixir.") {
        let without_prefix = &name[7..]; // Remove "Elixir."
        return without_prefix
            .split('.')
            .map(|s| camel_to_snake(s))
            .collect::<Vec<_>>()
            .join("_");
    }

    // Handle quoted atom names: 'Module.Name' -> module_name
    let name = name.trim_matches('\'');

    // Handle other dotted names
    if name.contains('.') {
        let sanitized = name
            .split('.')
            .map(|s| camel_to_snake(s))
            .collect::<Vec<_>>()
            .join("_");
        return sanitize_identifier(&sanitized);
    }

    // Regular name - just ensure it's a valid identifier
    sanitize_identifier(name)
}

/// Ensure a string is a valid Dream identifier.
fn sanitize_identifier(name: &str) -> String {
    let mut result = String::new();

    for (i, c) in name.chars().enumerate() {
        if c.is_ascii_alphanumeric() || c == '_' {
            // First char can't be a digit
            if i == 0 && c.is_ascii_digit() {
                result.push('_');
            }
            result.push(c.to_ascii_lowercase());
        } else if c == '-' || c == '.' {
            result.push('_');
        }
        // Skip other invalid characters
    }

    // Handle empty result
    if result.is_empty() {
        return "module_".to_string();
    }

    // Handle keywords by appending underscore
    if is_surreal_keyword(&result) {
        result.push('_');
    }

    result
}

/// Capitalize the first letter of a string (for struct names).
fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
    }
}

/// Check if a name is a Dream keyword that conflicts with function names.
/// Type names (atom, string, int, etc.) are NOT included because the parser
/// can distinguish `atom` (type) from `atom(...)` (function call).
fn is_surreal_function_keyword(name: &str) -> bool {
    matches!(
        name,
        "fn" | "let" | "mut" | "if" | "else" | "match" | "enum" | "mod" | "pub"
            | "self"
            | "spawn"
            | "receive"
            | "after"
            | "return"
            | "use"
            | "as"
            | "impl"
            | "trait"
            | "for"
            | "when"
            | "true"
            | "false"
            | "extern"
            | "type"
            | "struct"
    )
}

/// Sanitize a function name to be a valid Dream identifier.
/// Handles Elixir conventions:
/// - `encode!` → `encode_bang` (raises on error)
/// - `valid?` → `valid_q` (returns boolean)
///
/// Returns (sanitized_name, needs_name_attribute)
fn sanitize_function_name(name: &str) -> (String, bool) {
    let mut result = String::new();
    let mut modified = false;

    for c in name.chars() {
        match c {
            '!' => {
                result.push_str("_bang");
                modified = true;
            }
            '?' => {
                result.push_str("_q");
                modified = true;
            }
            c if c.is_ascii_alphanumeric() || c == '_' => {
                result.push(c);
            }
            '-' => {
                result.push('_');
                modified = true;
            }
            _ => {
                // Skip other invalid characters
                modified = true;
            }
        }
    }

    // Only rename actual keywords, not type names (parser distinguishes fn calls)
    if is_surreal_function_keyword(&result) {
        result.push('_');
        modified = true;
    }

    (result, modified)
}

/// Generate .surreal output from parsed module info.
fn generate_surreal(modules: &HashMap<String, ModuleInfo>) -> String {
    let mut output = String::new();
    output.push_str("// Generated by: surreal bindgen\n");
    output.push_str("// Do not edit manually.\n\n");

    let mut module_names: Vec<_> = modules.keys().collect();
    module_names.sort();

    // Generate commented type declarations for records and structs
    // NOTE: extern type syntax is not yet supported by the parser, so these are
    // generated as comments for documentation purposes. Future parser updates
    // could enable these as actual type declarations.
    let mut generated_types: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut has_type_comments = false;

    for module_name in &module_names {
        let info = &modules[*module_name];

        // Generate struct definitions for Erlang records with #[record] attribute
        for record in &info.records {
            let type_name = capitalize_first(&sanitize_identifier(&record.name));
            if generated_types.contains(&type_name) {
                continue;
            }

            // Generate struct with #[record = "name"] attribute
            output.push_str(&format!("#[record = \"{}\"]\n", record.name));
            output.push_str(&format!("pub struct {} {{\n", type_name));

            for (field_name, field_type) in &record.fields {
                let safe_field = sanitize_identifier(field_name);
                output.push_str(&format!(
                    "    {}: {},\n",
                    safe_field,
                    erlang_type_to_surreal(field_type)
                ));
            }

            output.push_str("}\n\n");
            generated_types.insert(type_name);
        }

        // Generate commented extern types for Elixir structs
        for struct_def in &info.structs {
            let type_name = sanitize_module_name(&struct_def.module);
            if generated_types.contains(&type_name) {
                continue;
            }

            if !has_type_comments {
                output.push_str("// Type declarations (for documentation - extern type not yet supported):\n");
                has_type_comments = true;
            }

            output.push_str(&format!("// Struct: {}\n", struct_def.module));
            output.push_str(&format!("// extern type {} = struct {{\n", type_name));

            for (field_name, field_type) in &struct_def.fields {
                let safe_field = sanitize_identifier(field_name);
                output.push_str(&format!(
                    "//     {}: {},\n",
                    safe_field,
                    erlang_type_to_surreal(field_type)
                ));
            }

            output.push_str("// };\n\n");
            generated_types.insert(type_name);
        }
    }

    if has_type_comments {
        output.push_str("\n");
    }

    // Then generate extern mod declarations with functions
    for module_name in module_names {
        let info = &modules[module_name];
        if info.specs.is_empty() {
            continue;
        }

        // Sanitize the module name to be a valid Dream identifier
        let safe_name = sanitize_module_name(module_name);

        output.push_str(&format!("// Erlang module: {}\n", module_name));

        // Generate #[name = "..."] attribute if the sanitized name differs from original
        if safe_name != *module_name {
            output.push_str(&format!("#[name = \"{}\"]\n", module_name));
        }

        output.push_str(&format!("extern mod {} {{\n", safe_name));

        for spec in &info.specs {
            let params: Vec<String> = spec
                .params
                .iter()
                .map(|(name, ty)| {
                    let safe_name = if is_surreal_keyword(name) {
                        format!("{}_", name)
                    } else {
                        name.clone()
                    };
                    format!("{}: {}", safe_name, erlang_type_to_surreal(ty))
                })
                .collect();

            let ret = erlang_type_to_surreal(&spec.return_type);

            // Sanitize function name (handles ! and ? suffixes from Elixir)
            let (safe_fn_name, needs_attr) = sanitize_function_name(&spec.name);

            // Add #[name = "..."] attribute if the function name was sanitized
            if needs_attr {
                output.push_str(&format!("    #[name = \"{}\"]\n", spec.name));
            }

            output.push_str(&format!(
                "    fn {}({}) -> {};\n",
                safe_fn_name,
                params.join(", "),
                ret
            ));
        }

        output.push_str("}\n\n");
    }

    output
}

/// Convert Erlang type to Dream type syntax.
///
/// Type naming convention:
/// - Primitives (lowercase): int, bool, float
/// - Compound/BEAM types (CamelCase): String, Binary, Atom, Pid, Ref, Map, Any
fn erlang_type_to_surreal(ty: &ErlangType) -> String {
    match ty {
        ErlangType::Named(name) => {
            match name.as_str() {
                // Primitives stay lowercase
                "integer" | "non_neg_integer" | "pos_integer" | "neg_integer" | "number" => "int".to_string(),
                "boolean" => "bool".to_string(),
                "float" => "float".to_string(),
                "byte" | "char" | "arity" => "int".to_string(),
                // Compound/BEAM types use CamelCase
                "atom" => "Atom".to_string(),
                "binary" | "bitstring" => "Binary".to_string(),
                "string" => "String".to_string(),
                "pid" => "Pid".to_string(),
                "reference" | "ref" => "Ref".to_string(),
                "map" => "Map".to_string(),
                "term" => "Any".to_string(),
                "iolist" | "iodata" => "IoList".to_string(),
                "no_return" | "none" => "Any".to_string(),
                // Handle already-CamelCased types
                "int" => "int".to_string(),
                "bool" => "bool".to_string(),
                "Atom" => "Atom".to_string(),
                "Binary" => "Binary".to_string(),
                "String" => "String".to_string(),
                "Pid" => "Pid".to_string(),
                "Ref" => "Ref".to_string(),
                "Map" => "Map".to_string(),
                "Any" => "Any".to_string(),
                "IoList" => "IoList".to_string(),
                _ => {
                    if name.is_empty() || name.contains(':') || name.contains('(') {
                        "Any".to_string()
                    } else if is_known_surreal_type(name) {
                        name.clone()
                    } else {
                        "Any".to_string()
                    }
                }
            }
        }
        ErlangType::AtomLiteral(_) => "Atom".to_string(),
        ErlangType::Var(_) => "Any".to_string(),
        ErlangType::Any => "Any".to_string(),
        ErlangType::Remote(_, _) => "Any".to_string(),
        ErlangType::List(inner) => {
            let inner_ty = erlang_type_to_surreal(inner);
            format!("[{}]", inner_ty)
        }
        ErlangType::Tuple(elements) => {
            if elements.is_empty() {
                "()".to_string()
            } else {
                let parts: Vec<String> = elements.iter().map(erlang_type_to_surreal).collect();
                format!("({})", parts.join(", "))
            }
        }
        ErlangType::Fun { params, ret } => {
            let param_types: Vec<String> = params.iter().map(erlang_type_to_surreal).collect();
            format!("fn({}) -> {}", param_types.join(", "), erlang_type_to_surreal(ret))
        }
        ErlangType::Union(types) => {
            // First try to detect Result/Option pattern
            let detected = detect_result_option_pattern(ErlangType::Union(types.clone()));
            if !matches!(detected, ErlangType::Union(_)) {
                return erlang_type_to_surreal(&detected);
            }

            // Filter out undefined for Option-like types
            let non_undefined: Vec<_> = types
                .iter()
                .filter(|t| !matches!(t,
                    ErlangType::AtomLiteral(n) | ErlangType::Named(n)
                    if n == "undefined" || n == "nil"
                ))
                .collect();

            if non_undefined.len() == 1 {
                erlang_type_to_surreal(non_undefined[0])
            } else {
                "Any".to_string()
            }
        }
        ErlangType::Result(ok_ty, err_ty) => {
            format!("Result<{}, {}>", erlang_type_to_surreal(ok_ty), erlang_type_to_surreal(err_ty))
        }
        ErlangType::Option(inner) => {
            format!("Option<{}>", erlang_type_to_surreal(inner))
        }
        ErlangType::Struct { module, fields: _ } => {
            // For now, emit the struct type name if known, otherwise Map
            if module.is_empty() {
                "Map".to_string()
            } else {
                // Use a type reference that will match the extern type
                sanitize_module_name(module)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_module_name() {
        assert_eq!(
            extract_module_name("-module(lists)."),
            Some("lists".to_string())
        );
        assert_eq!(
            extract_module_name("-module(erlang).\n-export([abs/1])."),
            Some("erlang".to_string())
        );
    }

    #[test]
    fn test_parse_simple_spec() {
        let registry = TypeRegistry::new();
        let spec = parse_single_spec("-spec abs(Number) -> integer() when Number :: integer().", &registry).unwrap();
        assert_eq!(spec.name, "abs");
        assert_eq!(spec.params.len(), 1);
    }

    #[test]
    fn test_parse_spec_no_when() {
        let registry = TypeRegistry::new();
        let spec = parse_single_spec("-spec self() -> pid().", &registry).unwrap();
        assert_eq!(spec.name, "self");
        assert_eq!(spec.params.len(), 0);
    }

    #[test]
    fn test_parse_list_type() {
        let registry = TypeRegistry::new();
        let spec = parse_single_spec("-spec reverse(List1) -> List2 when List1 :: [T], List2 :: [T].", &registry).unwrap();
        assert_eq!(spec.name, "reverse");
        assert_eq!(spec.params.len(), 1);
    }

    #[test]
    fn test_parse_fun_type() {
        let registry = TypeRegistry::new();
        let spec = parse_single_spec("-spec map(Fun, List1) -> List2 when Fun :: fun((A) -> B), List1 :: [A], List2 :: [B].", &registry).unwrap();
        assert_eq!(spec.name, "map");
        assert_eq!(spec.params.len(), 2);
    }

    #[test]
    fn test_parse_type_def() {
        let td = parse_single_type_def("-type filename() :: string().").unwrap();
        assert_eq!(td.name, "filename");
        assert!(td.params.is_empty());
        assert!(matches!(td.definition, ErlangType::Named(n) if n == "string"));
    }

    #[test]
    fn test_parse_type_def_with_params() {
        let td = parse_single_type_def("-type container(T) :: {ok, T} | {error, term()}.").unwrap();
        assert_eq!(td.name, "container");
        assert_eq!(td.params, vec!["T"]);
    }

    #[test]
    fn test_result_pattern_detection() {
        let ty = parse_type("{ok, binary()} | {error, atom()}");
        let detected = detect_result_option_pattern(ty);
        assert!(matches!(detected, ErlangType::Result(_, _)));
    }

    #[test]
    fn test_unit_result_pattern() {
        let ty = parse_type("ok | {error, Reason}");
        let detected = detect_result_option_pattern(ty);
        assert!(matches!(detected, ErlangType::Result(ok_ty, _) if matches!(*ok_ty, ErlangType::Tuple(ref v) if v.is_empty())));
    }

    #[test]
    fn test_option_pattern_detection() {
        let ty = parse_type("{ok, Value} | undefined");
        let detected = detect_result_option_pattern(ty);
        assert!(matches!(detected, ErlangType::Option(_)));
    }

    #[test]
    fn test_remote_type_parsing() {
        let ty = parse_type("calendar:datetime()");
        assert!(matches!(ty, ErlangType::Remote(m, n) if m == "calendar" && n == "datetime"));
    }

    #[test]
    fn test_result_type_to_surreal() {
        let ty = ErlangType::Result(
            Box::new(ErlangType::Named("binary".into())),
            Box::new(ErlangType::Named("atom".into())),
        );
        // CamelCase for compound types: Binary, Atom
        assert_eq!(erlang_type_to_surreal(&ty), "Result<Binary, Atom>");
    }

    #[test]
    fn test_option_type_to_surreal() {
        let ty = ErlangType::Option(Box::new(ErlangType::Named("string".into())));
        // CamelCase for compound types: String
        assert_eq!(erlang_type_to_surreal(&ty), "Option<String>");
    }

    #[test]
    fn test_sanitize_elixir_module_name() {
        // Elixir.Jason -> jason
        assert_eq!(sanitize_module_name("Elixir.Jason"), "jason");
        // Elixir.Jason.Encoder -> jason_encoder
        assert_eq!(sanitize_module_name("Elixir.Jason.Encoder"), "jason_encoder");
        // Elixir.Phoenix.Controller -> phoenix_controller
        assert_eq!(sanitize_module_name("Elixir.Phoenix.Controller"), "phoenix_controller");
        // CamelCase within segments: Elixir.Jason.EncodeError -> jason_encode_error
        assert_eq!(sanitize_module_name("Elixir.Jason.EncodeError"), "jason_encode_error");
        // Multiple words: Elixir.Phoenix.LiveView -> phoenix_live_view
        assert_eq!(sanitize_module_name("Elixir.Phoenix.LiveView"), "phoenix_live_view");
    }

    #[test]
    fn test_sanitize_regular_module_name() {
        // Regular Erlang modules unchanged
        assert_eq!(sanitize_module_name("lists"), "lists");
        assert_eq!(sanitize_module_name("erlang"), "erlang");
        assert_eq!(sanitize_module_name("file"), "file");
    }

    #[test]
    fn test_sanitize_keyword_module_name() {
        // Keywords get underscore suffix
        assert_eq!(sanitize_module_name("string"), "string_");
        assert_eq!(sanitize_module_name("binary"), "binary_");
    }

    #[test]
    fn test_sanitize_dotted_module_name() {
        // Other dotted names
        assert_eq!(sanitize_module_name("foo.bar"), "foo_bar");
        assert_eq!(sanitize_module_name("cowboy_req"), "cowboy_req");
    }

    #[test]
    fn test_sanitize_function_name_bang() {
        // Elixir ! suffix (functions that raise on error)
        let (name, needs_attr) = sanitize_function_name("encode!");
        assert_eq!(name, "encode_bang");
        assert!(needs_attr);

        let (name, needs_attr) = sanitize_function_name("decode!");
        assert_eq!(name, "decode_bang");
        assert!(needs_attr);
    }

    #[test]
    fn test_sanitize_function_name_question() {
        // Elixir ? suffix (functions that return boolean)
        let (name, needs_attr) = sanitize_function_name("valid?");
        assert_eq!(name, "valid_q");
        assert!(needs_attr);

        let (name, needs_attr) = sanitize_function_name("empty?");
        assert_eq!(name, "empty_q");
        assert!(needs_attr);
    }

    #[test]
    fn test_sanitize_function_name_normal() {
        // Normal function names should not need attribute
        let (name, needs_attr) = sanitize_function_name("encode");
        assert_eq!(name, "encode");
        assert!(!needs_attr);

        let (name, needs_attr) = sanitize_function_name("my_function");
        assert_eq!(name, "my_function");
        assert!(!needs_attr);

        // Type names are fine as function names (parser distinguishes by `(`)
        let (name, needs_attr) = sanitize_function_name("atom");
        assert_eq!(name, "atom");
        assert!(!needs_attr);

        let (name, needs_attr) = sanitize_function_name("string");
        assert_eq!(name, "string");
        assert!(!needs_attr);

        let (name, needs_attr) = sanitize_function_name("map");
        assert_eq!(name, "map");
        assert!(!needs_attr);

        let (name, needs_attr) = sanitize_function_name("list");
        assert_eq!(name, "list");
        assert!(!needs_attr);

        // struct is a keyword, so it needs to be renamed to struct_
        let (name, needs_attr) = sanitize_function_name("struct");
        assert_eq!(name, "struct_");
        assert!(needs_attr);
    }

    #[test]
    fn test_generate_function_name_attribute() {
        // Test that #[name = "..."] is generated for functions with ! or ?
        let mut modules = HashMap::new();
        modules.insert("Elixir.Jason".to_string(), ModuleInfo {
            specs: vec![
                ErlangSpec {
                    name: "encode".to_string(),
                    params: vec![("input".to_string(), ErlangType::Any)],
                    return_type: ErlangType::Any,
                },
                ErlangSpec {
                    name: "encode!".to_string(),
                    params: vec![("input".to_string(), ErlangType::Any)],
                    return_type: ErlangType::Any,
                },
            ],
            type_defs: Vec::new(),
            records: Vec::new(),
            structs: Vec::new(),
        });

        let output = generate_surreal(&modules);

        // Should have function name attribute for encode!
        assert!(output.contains("#[name = \"encode!\"]"),
            "Expected function #[name = \"encode!\"], got:\n{}", output);
        // Should use sanitized function name
        assert!(output.contains("fn encode_bang("),
            "Expected 'fn encode_bang', got:\n{}", output);
        // Regular encode should not have attribute
        assert!(output.contains("fn encode("),
            "Expected 'fn encode', got:\n{}", output);
    }

    #[test]
    fn test_generate_name_attribute() {
        // Test that #[name = "..."] is generated when names differ
        let mut modules = HashMap::new();
        modules.insert("Elixir.Jason".to_string(), ModuleInfo {
            specs: vec![ErlangSpec {
                name: "encode".to_string(),
                params: vec![("input".to_string(), ErlangType::Any)],
                return_type: ErlangType::Any,
            }],
            type_defs: Vec::new(),
            records: Vec::new(),
            structs: Vec::new(),
        });

        let output = generate_surreal(&modules);

        // Should contain #[name = "Elixir.Jason"]
        assert!(output.contains("#[name = \"Elixir.Jason\"]"),
            "Expected #[name = \"Elixir.Jason\"], got:\n{}", output);
        // Should use sanitized name 'jason'
        assert!(output.contains("extern mod jason"),
            "Expected 'extern mod jason', got:\n{}", output);
    }

    #[test]
    fn test_no_name_attribute_for_simple_modules() {
        // Test that #[name = "..."] is NOT generated when names match
        let mut modules = HashMap::new();
        modules.insert("lists".to_string(), ModuleInfo {
            specs: vec![ErlangSpec {
                name: "reverse".to_string(),
                params: vec![("list".to_string(), ErlangType::List(Box::new(ErlangType::Any)))],
                return_type: ErlangType::List(Box::new(ErlangType::Any)),
            }],
            type_defs: Vec::new(),
            records: Vec::new(),
            structs: Vec::new(),
        });

        let output = generate_surreal(&modules);

        // Should NOT contain #[name = "lists"]
        assert!(!output.contains("#[name ="),
            "Should not have #[name = ...] for simple module, got:\n{}", output);
        // Should use simple name
        assert!(output.contains("extern mod lists"),
            "Expected 'extern mod lists', got:\n{}", output);
    }

    #[test]
    fn test_extract_elixir_module_name() {
        assert_eq!(
            extract_elixir_module_name("defmodule Jason do\nend"),
            Some("Elixir.Jason".to_string())
        );
        assert_eq!(
            extract_elixir_module_name("defmodule Jason.Encoder do\nend"),
            Some("Elixir.Jason.Encoder".to_string())
        );
        assert_eq!(
            extract_elixir_module_name("defmodule Phoenix.Controller do\n  use Phoenix\nend"),
            Some("Elixir.Phoenix.Controller".to_string())
        );
    }

    #[test]
    fn test_parse_elixir_spec() {
        let registry = TypeRegistry::new();
        let spec = parse_elixir_single_spec(
            "@spec encode(term()) :: {:ok, String.t()} | {:error, Exception.t()}",
            &registry
        ).unwrap();
        assert_eq!(spec.name, "encode");
        assert_eq!(spec.params.len(), 1);
        assert!(matches!(spec.return_type, ErlangType::Result(_, _)));
    }

    #[test]
    fn test_parse_elixir_spec_with_named_args() {
        let registry = TypeRegistry::new();
        let spec = parse_elixir_single_spec(
            "@spec decode(input :: binary()) :: {:ok, term()} | {:error, Exception.t()}",
            &registry
        ).unwrap();
        assert_eq!(spec.name, "decode");
        assert_eq!(spec.params.len(), 1);
        assert_eq!(spec.params[0].0, "input");
    }

    #[test]
    fn test_parse_elixir_type() {
        assert!(matches!(parse_elixir_type("String.t()"), ErlangType::Named(n) if n == "string"));
        assert!(matches!(parse_elixir_type("integer()"), ErlangType::Named(n) if n == "int"));
        assert!(matches!(parse_elixir_type(":ok"), ErlangType::AtomLiteral(n) if n == "ok"));
        assert!(matches!(parse_elixir_type("[binary()]"), ErlangType::List(_)));
    }

    #[test]
    fn test_parse_elixir_type_def() {
        let td = parse_elixir_single_type_def("@type options :: keyword()").unwrap();
        assert_eq!(td.name, "options");
        assert!(td.params.is_empty());
    }

    #[test]
    fn test_parse_elixir_result_pattern() {
        let ty = parse_elixir_type("{:ok, String.t()} | {:error, Exception.t()}");
        assert!(matches!(ty, ErlangType::Result(_, _)));
    }

    #[test]
    fn test_parse_erlang_record_simple() {
        let records = parse_erlang_records("-record(person, {name, age}).");
        assert_eq!(records.len(), 1);
        assert_eq!(records[0].name, "person");
        assert_eq!(records[0].fields.len(), 2);
        assert_eq!(records[0].fields[0].0, "name");
        assert_eq!(records[0].fields[1].0, "age");
    }

    #[test]
    fn test_parse_erlang_record_with_types() {
        let records = parse_erlang_records("-record(user, {name :: string(), age :: integer()}).");
        assert_eq!(records.len(), 1);
        assert_eq!(records[0].name, "user");
        assert_eq!(records[0].fields.len(), 2);
        assert_eq!(records[0].fields[0].0, "name");
        assert!(matches!(records[0].fields[0].1, ErlangType::Named(ref n) if n == "string"));
        assert_eq!(records[0].fields[1].0, "age");
        assert!(matches!(records[0].fields[1].1, ErlangType::Named(ref n) if n == "int"));
    }

    #[test]
    fn test_parse_erlang_record_with_defaults() {
        let records = parse_erlang_records("-record(config, {timeout = 5000 :: integer(), host :: string()}).");
        assert_eq!(records.len(), 1);
        assert_eq!(records[0].name, "config");
        assert_eq!(records[0].fields.len(), 2);
        assert_eq!(records[0].fields[0].0, "timeout");
        assert_eq!(records[0].fields[1].0, "host");
    }

    #[test]
    fn test_parse_elixir_defstruct_list_style() {
        let structs = parse_elixir_structs("defstruct [:name, :age]", "Elixir.Person");
        assert_eq!(structs.len(), 1);
        assert_eq!(structs[0].module, "Person");
        assert_eq!(structs[0].fields.len(), 2);
        assert_eq!(structs[0].fields[0].0, "name");
        assert_eq!(structs[0].fields[1].0, "age");
    }

    #[test]
    fn test_parse_elixir_defstruct_keyword_style() {
        let structs = parse_elixir_structs("defstruct name: nil, age: 0", "Elixir.User");
        assert_eq!(structs.len(), 1);
        assert_eq!(structs[0].module, "User");
        assert_eq!(structs[0].fields.len(), 2);
        assert_eq!(structs[0].fields[0].0, "name");
        assert_eq!(structs[0].fields[1].0, "age");
    }

    #[test]
    fn test_parse_elixir_struct_type() {
        let ty = parse_elixir_type("%User{name: String.t(), age: integer()}");
        assert!(matches!(ty, ErlangType::Struct { ref module, ref fields }
            if module == "User" && fields.len() == 2));
    }

    #[test]
    fn test_parse_elixir_module_struct_type() {
        let ty = parse_elixir_type("%__MODULE__{message: String.t()}");
        assert!(matches!(ty, ErlangType::Struct { ref module, ref fields }
            if module.is_empty() && fields.len() == 1));
    }

    #[test]
    fn test_generate_extern_type_for_record() {
        let mut modules = HashMap::new();
        modules.insert("test".to_string(), ModuleInfo {
            specs: Vec::new(),
            type_defs: Vec::new(),
            records: vec![ErlangRecord {
                name: "person".to_string(),
                fields: vec![
                    ("name".to_string(), ErlangType::Named("string".to_string())),
                    ("age".to_string(), ErlangType::Named("int".to_string())),
                ],
            }],
            structs: Vec::new(),
        });

        let output = generate_surreal(&modules);
        // Records are now generated as actual structs with #[record] attribute
        assert!(output.contains("#[record = \"person\"]"),
            "Expected #[record] attribute, got:\n{}", output);
        assert!(output.contains("pub struct Person {"),
            "Expected struct declaration with capitalized name, got:\n{}", output);
        assert!(output.contains("name: String,"),
            "Expected name field, got:\n{}", output);
        assert!(output.contains("age: int,"),
            "Expected age field (int stays lowercase), got:\n{}", output);
    }

    #[test]
    fn test_generate_extern_type_for_struct() {
        let mut modules = HashMap::new();
        modules.insert("Elixir.Jason.Error".to_string(), ModuleInfo {
            specs: Vec::new(),
            type_defs: Vec::new(),
            records: Vec::new(),
            structs: vec![ElixirStruct {
                module: "Jason.Error".to_string(),
                fields: vec![
                    ("message".to_string(), ErlangType::Named("string".to_string())),
                ],
            }],
        });

        let output = generate_surreal(&modules);
        // Types are generated as comments since extern type isn't supported yet
        assert!(output.contains("// extern type jason_error = struct {"),
            "Expected commented extern type declaration, got:\n{}", output);
        assert!(output.contains("//     message: String,"),
            "Expected message field, got:\n{}", output);
    }
}

#[cfg(test)]
mod test_map_parsing {
    use super::*;
    
    #[test]
    fn test_parse_erlang_map_type() {
        let ty = parse_type("#{method := binary()}");
        println!("Parsed type: {:?}", ty);
        assert!(matches!(ty, ErlangType::Named(ref n) if n == "map"), 
            "Expected Named(map), got {:?}", ty);
    }
    
    #[test]
    fn test_type_resolution_for_req() {
        // Parse a type definition like -type req() :: #{...}
        let td = parse_single_type_def("-type req() :: #{method := binary()}.").unwrap();
        println!("Type def: {:?}", td);
        assert_eq!(td.name, "req");
        assert!(matches!(td.definition, ErlangType::Named(ref n) if n == "map"),
            "Expected definition to be Named(map), got {:?}", td.definition);
        
        // Register and resolve
        let mut registry = TypeRegistry::new();
        registry.register(td);
        
        let ty = parse_type("req()");
        println!("Before resolve: {:?}", ty);
        let resolved = resolve_type_refs(&ty, &registry);
        println!("After resolve: {:?}", resolved);
        assert!(matches!(resolved, ErlangType::Named(ref n) if n == "map"),
            "Expected resolved to be Named(map), got {:?}", resolved);
    }
}

#[cfg(test)]
mod test_multiline {
    use super::*;
    
    #[test]
    fn test_multiline_map_type_def() {
        let source = r#"
-type req() :: #{
    method := binary(),
    path := binary()
}.

-spec method(req()) -> binary().
method(Req) -> maps:get(method, Req).
"#;
        
        let type_defs = parse_type_defs(source);
        println!("Type defs: {:?}", type_defs);
        assert!(!type_defs.is_empty(), "Should have parsed type defs");
        
        let req_def = type_defs.iter().find(|td| td.name == "req");
        println!("req type def: {:?}", req_def);
        assert!(req_def.is_some(), "Should have found req type");
        
        let req_def = req_def.unwrap();
        assert!(matches!(req_def.definition, ErlangType::Named(ref n) if n == "map"),
            "Expected req() to be a map, got {:?}", req_def.definition);
    }
}

#[cfg(test)]
mod test_full_flow {
    use super::*;
    
    #[test]
    fn test_full_spec_resolution() {
        let source = r#"
-type req() :: #{
    method := binary(),
    path := binary()
}.

-spec method(req()) -> binary().
method(Req) -> maps:get(method, Req).
"#;
        
        let type_defs = parse_type_defs(source);
        let mut registry = TypeRegistry::new();
        for td in &type_defs {
            registry.register(td.clone());
        }
        
        let specs = parse_specs(source, &registry);
        println!("Specs: {:?}", specs);
        
        let method_spec = specs.iter().find(|s| s.name == "method");
        assert!(method_spec.is_some(), "Should have found method spec");
        
        let method_spec = method_spec.unwrap();
        println!("Method spec params: {:?}", method_spec.params);
        
        // The param type should be resolved to map, then to Map
        let param_type = &method_spec.params[0].1;
        let surreal_type = erlang_type_to_surreal(param_type);
        println!("Surreal type: {}", surreal_type);
        assert_eq!(surreal_type, "Map", "Expected Map, got {}", surreal_type);
    }
}
