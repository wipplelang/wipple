use super::*;

impl<L: Loader> Compiler<L> {
    pub(super) fn load_builtins(&mut self, scope: &Scope, info: &mut Info) {
        let builtin_span = Span::new(FilePath::_Builtin, 0..0);

        let mut scope_values = scope.values.borrow_mut();

        macro_rules! add {
            ($decls:ident, $name:expr, $id:expr, $scope_value:expr, $value:expr $(,)?) => {{
                let name = InternedString::new($name);
                let id = $id;

                info.declarations.$decls.insert(
                    id,
                    Declaration::Builtin(DeclarationKind {
                        name,
                        span: builtin_span,
                        value: $value,
                    }),
                );

                scope_values.insert(name, $scope_value(id));
            }};
        }

        add!(
            builtin_types,
            "Number",
            BuiltinType::Number,
            ScopeValue::BuiltinType,
            (),
        );

        add!(
            builtin_types,
            "Text",
            BuiltinType::Text,
            ScopeValue::BuiltinType,
            (),
        );

        add!(
            builtin_types,
            "List",
            BuiltinType::List,
            ScopeValue::BuiltinType,
            (),
        );
    }
}
