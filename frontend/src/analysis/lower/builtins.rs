use super::*;

impl Compiler<'_> {
    pub(super) fn load_builtins(&self, scope: &Scope, info: &mut Info) {
        let mut scope_values = scope.values.borrow_mut();

        macro_rules! add {
            ($kind:ident, $span:expr, $name:expr, $value:expr $(,)?) => {{
                paste::paste! {
                    let name = InternedString::new($name);
                    let id = self.[<new_ $kind _id>](info.file);

                    info.declarations.[<$kind s>].insert(
                        id,
                        Declaration {
                            name: Some(name),
                            span: $span,
                            value: $value,
                        },
                    );

                    scope_values.insert(name, ScopeValue::[<$kind:camel>](id));
                }
            }};
        }

        add!(
            builtin_type,
            Span::builtin("`!` type"),
            "!",
            BuiltinType {
                kind: BuiltinTypeKind::Never,
                attributes: DeclarationAttributes {
                    help: vec![InternedString::new(
                        "The type of expressions which always diverge (eg. `end`) or crash the program (eg. `crash`). Because there are no values of this type, you may use a `!` expression in place of any other expression. For example, you can use `...` (which has type `!`) to indicate unfinished code.",
                    )],
                    ..Default::default()
                }
            },
        );

        add!(
            builtin_type,
            Span::builtin("`Number` type"),
            "Number",
            BuiltinType {
                kind: BuiltinTypeKind::Number,
                attributes: DeclarationAttributes {
                    help: vec![InternedString::new("Represents a decimal number.",)],
                    ..Default::default()
                }
            },
        );

        add!(
            builtin_type,
            Span::builtin("`Integer` type"),
            "Integer",
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
            builtin_type,
            Span::builtin("`Natural` type"),
            "Natural",
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
            builtin_type,
            Span::builtin("`Byte` type"),
            "Byte",
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
            builtin_type,
            Span::builtin("`Signed` type"),
            "Signed",
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
            builtin_type,
            Span::builtin("`Unsigned` type"),
            "Unsigned",
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
            builtin_type,
            Span::builtin("`Float` type"),
            "Float",
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
            builtin_type,
            Span::builtin("`Double` type"),
            "Double",
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
            builtin_type,
            Span::builtin("`Text` type"),
            "Text",
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
            builtin_type,
            Span::builtin("`List` type"),
            "List",
            BuiltinType {
                kind: BuiltinTypeKind::List,
                attributes: DeclarationAttributes {
                    help: vec![InternedString::new(
                        "Represents a collection of values. `List` is generic, meaning you can use any value inside a list (as long as all the values have the same type). You can create a list using the list function: `list (1 , 2 , 3)`.",
                    )],
                    ..Default::default()
                }
            },
        );

        add!(
            builtin_type,
            Span::builtin("`Mutable` type"),
            "Mutable",
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
