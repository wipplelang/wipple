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
                    doc: VecDeque::from([InternedString::new(
                        "The unit type; contains no information. `()` is usually seen as the result of functions which have an effect but produce no meaningful value.",
                    )]),
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
                    doc: VecDeque::from([InternedString::new("")]),
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
                    doc: VecDeque::from([InternedString::new("")]),
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
                    doc: VecDeque::from([InternedString::new("")]),
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
                    doc: VecDeque::from([InternedString::new("")]),
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
                    doc: VecDeque::from([InternedString::new("")]),
                    ..Default::default()
                }
            },
        );
    }
}
