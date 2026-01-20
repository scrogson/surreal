//! Snapshot tests for compiler errors and successful compilation.
//!
//! These tests use insta to snapshot compiler output, ensuring we don't
//! accidentally break error messages or introduce regressions.

use surreal::compiler::{check_module, Parser, TypeError};
use std::fs;
use std::path::Path;

/// Format a type error for snapshot testing
fn format_type_error(err: &TypeError) -> String {
    if let Some(help) = &err.help {
        format!("error: {}\n  help: {}", err.message, help)
    } else {
        format!("error: {}", err.message)
    }
}

/// Compile a Dream source file and return the result as a string
fn compile_to_string(source: &str) -> String {
    // Parse
    let mut parser = Parser::new(source);
    let module = match parser.parse_module() {
        Ok(m) => m,
        Err(e) => return format!("parse error: {}", e.message),
    };

    // Type check
    match check_module(&module) {
        Ok(()) => "success".to_string(),
        Err(e) => format_type_error(&e),
    }
}

#[test]
fn test_error_snapshots() {
    let fixtures_dir = Path::new("tests/fixtures/errors");

    if !fixtures_dir.exists() {
        panic!("Fixtures directory not found: {:?}", fixtures_dir);
    }

    for entry in fs::read_dir(fixtures_dir).expect("Failed to read fixtures directory") {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();

        if path.extension().map(|s| s == "surreal").unwrap_or(false) {
            let source = fs::read_to_string(&path).expect("Failed to read fixture");
            let filename = path.file_stem().unwrap().to_str().unwrap();

            let result = compile_to_string(&source);

            insta::assert_snapshot!(format!("error_{}", filename), result);
        }
    }
}

#[test]
fn test_valid_snapshots() {
    let fixtures_dir = Path::new("tests/fixtures/valid");

    if !fixtures_dir.exists() {
        panic!("Fixtures directory not found: {:?}", fixtures_dir);
    }

    for entry in fs::read_dir(fixtures_dir).expect("Failed to read fixtures directory") {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();

        if path.extension().map(|s| s == "surreal").unwrap_or(false) {
            let source = fs::read_to_string(&path).expect("Failed to read fixture");
            let filename = path.file_stem().unwrap().to_str().unwrap();

            let result = compile_to_string(&source);

            // Valid files should compile successfully
            insta::assert_snapshot!(format!("valid_{}", filename), result);
        }
    }
}
