//! Prelude module - types automatically available in all Dream modules.
//!
//! The prelude contains fundamental types like Option and Result that are
//! injected into every module at parse time.

use crate::compiler::ast::{EnumDef, EnumVariant, Item, SpannedType, Type, TypeParam, VariantKind};

/// Generate the prelude items to be injected into every module.
/// Returns Option<T> and Result<T, E> enum definitions.
pub fn prelude_items() -> Vec<Item> {
    vec![
        // enum Option<T> { Some(T), None }
        Item::Enum(EnumDef {
            attrs: vec![],
            name: "Option".to_string(),
            type_params: vec![TypeParam { name: "T".to_string(), bounds: vec![] }],
            variants: vec![
                EnumVariant {
                    name: "Some".to_string(),
                    kind: VariantKind::Tuple(vec![SpannedType::unspanned(Type::TypeVar("T".to_string()))]),
                },
                EnumVariant {
                    name: "None".to_string(),
                    kind: VariantKind::Unit,
                },
            ],
            is_pub: false, // Private - each module gets its own copy
            span: 0..0,
        }),
        // enum Result<T, E> { Ok(T), Err(E) }
        Item::Enum(EnumDef {
            attrs: vec![],
            name: "Result".to_string(),
            type_params: vec![
                TypeParam { name: "T".to_string(), bounds: vec![] },
                TypeParam { name: "E".to_string(), bounds: vec![] },
            ],
            variants: vec![
                EnumVariant {
                    name: "Ok".to_string(),
                    kind: VariantKind::Tuple(vec![SpannedType::unspanned(Type::TypeVar("T".to_string()))]),
                },
                EnumVariant {
                    name: "Err".to_string(),
                    kind: VariantKind::Tuple(vec![SpannedType::unspanned(Type::TypeVar("E".to_string()))]),
                },
            ],
            is_pub: false, // Private - each module gets its own copy
            span: 0..0,
        }),
    ]
}

/// Check if a module already defines a type with the given name.
pub fn module_defines_type(items: &[Item], type_name: &str) -> bool {
    items.iter().any(|item| match item {
        Item::Enum(def) => def.name == type_name,
        Item::Struct(def) => def.name == type_name,
        _ => false,
    })
}

/// Get prelude items that should be injected, excluding any already defined.
pub fn prelude_items_for_module(existing_items: &[Item]) -> Vec<Item> {
    prelude_items()
        .into_iter()
        .filter(|item| {
            let name = match item {
                Item::Enum(def) => &def.name,
                Item::Struct(def) => &def.name,
                _ => return true,
            };
            !module_defines_type(existing_items, name)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_prelude_items() {
        let items = prelude_items();
        assert_eq!(items.len(), 2);

        // Check Option
        if let Item::Enum(opt) = &items[0] {
            assert_eq!(opt.name, "Option");
            assert_eq!(opt.type_params.len(), 1);
            assert_eq!(opt.type_params[0].name, "T");
            assert_eq!(opt.variants.len(), 2);
            assert_eq!(opt.variants[0].name, "Some");
            assert_eq!(opt.variants[1].name, "None");
        } else {
            panic!("Expected Option enum");
        }

        // Check Result
        if let Item::Enum(res) = &items[1] {
            assert_eq!(res.name, "Result");
            assert_eq!(res.type_params.len(), 2);
            assert_eq!(res.type_params[0].name, "T");
            assert_eq!(res.type_params[1].name, "E");
            assert_eq!(res.variants.len(), 2);
            assert_eq!(res.variants[0].name, "Ok");
            assert_eq!(res.variants[1].name, "Err");
        } else {
            panic!("Expected Result enum");
        }
    }

    #[test]
    fn test_excludes_already_defined() {
        let existing = vec![Item::Enum(EnumDef {
            attrs: vec![],
            name: "Option".to_string(),
            type_params: vec![TypeParam { name: "T".to_string(), bounds: vec![] }],
            variants: vec![],
            is_pub: false,
            span: 0..0,
        })];

        let items = prelude_items_for_module(&existing);
        assert_eq!(items.len(), 1); // Only Result should be injected

        if let Item::Enum(res) = &items[0] {
            assert_eq!(res.name, "Result");
        } else {
            panic!("Expected Result enum");
        }
    }
}
