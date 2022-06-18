#![allow(clippy::needless_update)] // for future-proofing

use super::*;

impl<L> Compiler<L> {
    pub(super) fn load_builtins(&mut self, scope: &Scope, info: &mut Info) {
        let builtin_span = Span::new(FilePath::_Builtin, 0..0);

        let mut scope_values = scope.values.borrow_mut();

        macro_rules! add {
            ($decls:ident, $name:expr, $id:expr, $scope_value:expr, $value:expr $(,)?) => {{
                let name = InternedString::new($name);
                let id = $id;

                info.declarations.$decls.insert(
                    id,
                    Declaration {
                        name: Some(name),
                        span: builtin_span,
                        value: $value,
                    },
                );

                scope_values.insert(name, $scope_value(id));
            }};
        }

        add!(
            builtin_types,
            "()",
            self.new_builtin_type_id(),
            ScopeValue::BuiltinType,
            BuiltinType {
                kind: BuiltinTypeKind::Unit,
                attributes: DeclarationAttributes {
                    help: vec![InternedString::new(
                        "The unit type; contains no information. `()` is usually seen as the result of functions which have an effect but produce no meaningful value.",
                    )],
                    ..Default::default()
                },
            },
        );

        add!(
            builtin_types,
            "!",
            self.new_builtin_type_id(),
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
            "Number",
            self.new_builtin_type_id(),
            ScopeValue::BuiltinType,
            BuiltinType {
                kind: BuiltinTypeKind::Number,
                attributes: DeclarationAttributes {
                    help: vec![InternedString::new(
                        "Represents a number. Unlike in most languages, Wipple numbers are stored in decimal format instead of floating-point format, making calculations with base-10 numbers more accurate.",
                    )],
                    ..Default::default()
                }
            },
        );

        add!(
            builtin_types,
            "Text",
            self.new_builtin_type_id(),
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
            "List",
            self.new_builtin_type_id(),
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
            "Mutable",
            self.new_builtin_type_id(),
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
