use super::*;
use crate::Loader;
use lazy_static::lazy_static;
use std::sync::{Arc, Mutex};

impl<L: Loader> Compiler<L> {
    pub(super) fn load_builtins(&mut self, scope: &Scope, info: &mut Info) {
        let mut scope_values = scope.values.borrow_mut();

        macro_rules! add {
            ($decls:ident, $span:expr, $name:expr, $id_storage:ident: $id_ty:ty = $id:expr, $scope_value:expr, $value:expr $(,)?) => {{
                lazy_static! {
                    static ref $id_storage: Arc<Mutex<Option<$id_ty>>> = Default::default();
                }

                let name = InternedString::new($name);
                let id = *$id_storage.lock().unwrap().get_or_insert_with(|| $id);

                info.declarations.$decls.insert(
                    id,
                    Declaration {
                        name: Some(name),
                        span: $span,
                        value: $value,
                        uses: Vec::new(),
                    },
                );

                scope_values.insert(name, $scope_value(id));
            }};
        }

        add!(
            builtin_types,
            Span::builtin("`!` type"),
            "!",
            NEVER_TYPE: BuiltinTypeId = self.new_builtin_type_id(),
            ScopeValue::BuiltinType,
            BuiltinType {
                kind: BuiltinTypeKind::Never,
                attributes: DeclarationAttributes {
                    help: vec![InternedString::new(
                        "The type of expressions which always diverge (eg. `return`) or crash the program (eg. `crash`). Because there are no values of this type, you may use a `!` expression in place of any other expression. For example, you can use `...` to indicate unfinished code.",
                    )],
                    ..Default::default()
                }
            },
        );

        add!(
            builtin_types,
            Span::builtin("`Number` type"),
            "Number",
            NUMBER_TYPE: BuiltinTypeId = self.new_builtin_type_id(),
            ScopeValue::BuiltinType,
            BuiltinType {
                kind: BuiltinTypeKind::Number,
                attributes: DeclarationAttributes {
                    help: vec![InternedString::new("Represents a decimal number.",)],
                    ..Default::default()
                }
            },
        );

        add!(
            builtin_types,
            Span::builtin("`Integer` type"),
            "Integer",
            INTEGER_TYPE: BuiltinTypeId = self.new_builtin_type_id(),
            ScopeValue::BuiltinType,
            BuiltinType {
                kind: BuiltinTypeKind::Integer,
                attributes: DeclarationAttributes {
                    help: vec![InternedString::new(
                        "Represents a whole number that can be positive or negative.",
                    )],
                    ..Default::default()
                }
            },
        );

        add!(
            builtin_types,
            Span::builtin("`Natural` type"),
            "Natural",
            NATURAL_TYPE: BuiltinTypeId = self.new_builtin_type_id(),
            ScopeValue::BuiltinType,
            BuiltinType {
                kind: BuiltinTypeKind::Natural,
                attributes: DeclarationAttributes {
                    help: vec![InternedString::new(
                        "Represents a whole number that can only be positive.",
                    )],
                    ..Default::default()
                }
            },
        );

        add!(
            builtin_types,
            Span::builtin("`Byte` type"),
            "Byte",
            BYTE_TYPE: BuiltinTypeId = self.new_builtin_type_id(),
            ScopeValue::BuiltinType,
            BuiltinType {
                kind: BuiltinTypeKind::Byte,
                attributes: DeclarationAttributes {
                    help: vec![InternedString::new(
                        "Represents a byte of data, specifically an 8-bit unsigned integer.",
                    )],
                    ..Default::default()
                }
            },
        );

        add!(
            builtin_types,
            Span::builtin("`Signed` type"),
            "Signed",
            SIGNED_TYPE: BuiltinTypeId = self.new_builtin_type_id(),
            ScopeValue::BuiltinType,
            BuiltinType {
                kind: BuiltinTypeKind::Signed,
                attributes: DeclarationAttributes {
                    help: vec![InternedString::new(
                        "Represents a platform-sized signed integer, equivalent to the `signed int` type in C.",
                    )],
                    ..Default::default()
                }
            },
        );

        add!(
            builtin_types,
            Span::builtin("`Unsigned` type"),
            "Unsigned",
            UNSIGNED_TYPE: BuiltinTypeId = self.new_builtin_type_id(),
            ScopeValue::BuiltinType,
            BuiltinType {
                kind: BuiltinTypeKind::Unsigned,
                attributes: DeclarationAttributes {
                    help: vec![InternedString::new(
                        "Represents a platform-sized unsigned integer, equivalent to the `unsigned int` type in C.",
                    )],
                    ..Default::default()
                }
            },
        );

        add!(
            builtin_types,
            Span::builtin("`Float` type"),
            "Float",
            FLOAT_TYPE: BuiltinTypeId = self.new_builtin_type_id(),
            ScopeValue::BuiltinType,
            BuiltinType {
                kind: BuiltinTypeKind::Float,
                attributes: DeclarationAttributes {
                    help: vec![InternedString::new(
                        "Represents a 32-bit floating-point number, equivalent to the `float` type in C.",
                    )],
                    ..Default::default()
                }
            },
        );

        add!(
            builtin_types,
            Span::builtin("`Double` type"),
            "Double",
            DOUBLE_TYPE: BuiltinTypeId = self.new_builtin_type_id(),
            ScopeValue::BuiltinType,
            BuiltinType {
                kind: BuiltinTypeKind::Double,
                attributes: DeclarationAttributes {
                    help: vec![InternedString::new(
                        "Represents a 64-bit floating-point number, equivalent to the `double` type in C.",
                    )],
                    ..Default::default()
                }
            },
        );

        add!(
            builtin_types,
            Span::builtin("`Text` type"),
            "Text",
            TEXT_TYPE: BuiltinTypeId = self.new_builtin_type_id(),
            ScopeValue::BuiltinType,
            BuiltinType {
                kind: BuiltinTypeKind::Text,
                attributes: DeclarationAttributes {
                    help: vec![InternedString::new(
                        "Represents a collection of characters. In Wipple, text is stored in UTF-8 format.",
                    )],
                    ..Default::default()
                }
            },
        );

        add!(
            builtin_types,
            Span::builtin("`List` type"),
            "List",
            LIST_TYPE: BuiltinTypeId = self.new_builtin_type_id(),
            ScopeValue::BuiltinType,
            BuiltinType {
                kind: BuiltinTypeKind::List,
                attributes: DeclarationAttributes {
                    help: vec![InternedString::new(
                        "Represents a collection of values. `List` is generic, meaning you can use any value inside a list (as long as all the values have the same type). You can create a list using the list literal: `'(1 2 3)`.",
                    )],
                    ..Default::default()
                }
            },
        );

        add!(
            builtin_types,
            Span::builtin("`Mutable` type"),
            "Mutable",
            MUTABLE_TYPE: BuiltinTypeId = self.new_builtin_type_id(),
            ScopeValue::BuiltinType,
            BuiltinType {
                kind: BuiltinTypeKind::Mutable,
                attributes: DeclarationAttributes {
                    help: vec![InternedString::new(
                        "A container for a value that can change at runtime. Functions that change a `Mutable` value end in `!`. You should generally use functions that return a new value instead of functions that mutate their input, but `Mutable` is useful for improving performance or storing global state.",
                    )],
                    ..Default::default()
                }
            },
        );
    }
}
