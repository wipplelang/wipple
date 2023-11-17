use super::*;

impl Lowerer {
    pub fn load_builtins(&mut self, scope: &ScopeSet) {
        macro_rules! add {
            ($kind:ident, $span:expr, $name:expr, $value:expr $(,)?) => {{
                paste::paste! {
                    let name = InternedString::new($name);
                    let id = self.compiler.[<new_ $kind _id_in>](self.file);

                    self.declarations.[<$kind s>].insert(
                        id,
                        Declaration::resolved(Some(name), $span, $value),
                    );

                    self.insert(name, AnyDeclaration::[<$kind:camel>](id), scope);
                }
            }};
        }

        add!(
            builtin_type,
            Span::builtin(),
            "Number",
            BuiltinTypeDeclaration {
                kind: BuiltinTypeDeclarationKind::Number,
                attributes: DeclarationAttributes {
                    help: vec![InternedString::new("Represents a decimal number.",)],
                    ..Default::default()
                }
            },
        );

        add!(
            builtin_type,
            Span::builtin(),
            "Integer",
            BuiltinTypeDeclaration {
                kind: BuiltinTypeDeclarationKind::Integer,
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
            Span::builtin(),
            "Natural",
            BuiltinTypeDeclaration {
                kind: BuiltinTypeDeclarationKind::Natural,
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
            Span::builtin(),
            "Byte",
            BuiltinTypeDeclaration {
                kind: BuiltinTypeDeclarationKind::Byte,
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
            Span::builtin(),
            "Signed",
            BuiltinTypeDeclaration {
                kind: BuiltinTypeDeclarationKind::Signed,
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
            Span::builtin(),
            "Unsigned",
            BuiltinTypeDeclaration {
                kind: BuiltinTypeDeclarationKind::Unsigned,
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
            Span::builtin(),
            "Float",
            BuiltinTypeDeclaration {
                kind: BuiltinTypeDeclarationKind::Float,
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
            Span::builtin(),
            "Double",
            BuiltinTypeDeclaration {
                kind: BuiltinTypeDeclarationKind::Double,
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
            Span::builtin(),
            "Text",
            BuiltinTypeDeclaration {
                kind: BuiltinTypeDeclarationKind::Text,
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
            Span::builtin(),
            "List",
            BuiltinTypeDeclaration {
                kind: BuiltinTypeDeclarationKind::List,
                attributes: DeclarationAttributes {
                    help: vec![InternedString::new(
                        "Represents a collection of values. `List` is generic, meaning you can use any value inside a list (as long as all the values have the same type). You can create a list using the `list` function: `list 1 2 3`.",
                    )],
                    ..Default::default()
                }
            },
        );

        add!(
            builtin_type,
            Span::builtin(),
            "Reference",
            BuiltinTypeDeclaration {
                kind: BuiltinTypeDeclarationKind::Reference,
                attributes: DeclarationAttributes {
                    help: vec![InternedString::new(
                        "A shared reference to a value. Changing the value contained in a `Reference` will change it for all holders of the same `Reference`.",
                    )],
                    ..Default::default()
                }
            },
        );

        add!(
            builtin_type,
            Span::builtin(),
            "UI",
            BuiltinTypeDeclaration {
                kind: BuiltinTypeDeclarationKind::Ui,
                attributes: DeclarationAttributes {
                    help: vec![InternedString::new("A handle for a UI element.")],
                    ..Default::default()
                }
            },
        );

        add!(
            builtin_type,
            Span::builtin(),
            "Task-Group",
            BuiltinTypeDeclaration {
                kind: BuiltinTypeDeclarationKind::TaskGroup,
                attributes: DeclarationAttributes {
                    help: vec![InternedString::new(
                        "A way to manage multiple sections of code executing at the same time. Use the `task-group` function to create a new task group, and use `task` to add a task to the group. You can then `wait` on the task group to wait for all the tasks to finish before continuing the program.",
                    )],
                    ..Default::default()
                }
            },
        );

        add!(
            builtin_type,
            Span::builtin(),
            "Hasher",
            BuiltinTypeDeclaration {
                kind: BuiltinTypeDeclarationKind::Hasher,
                attributes: DeclarationAttributes {
                    help: vec![InternedString::new("Contains the hashed components of a value implementing `Hash`.")],
                    ..Default::default()
                }
            },
        );
    }
}
