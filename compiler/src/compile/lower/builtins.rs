use super::*;
use crate::compile::typecheck::BUILTIN_TYPES;

pub(super) fn load_builtins(scope: &Scope, info: &mut Info) {
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
        types,
        "Number",
        BUILTIN_TYPES.number.id().unwrap(),
        ScopeValue::Type,
        Type {
            parameters: Vec::new(),
            kind: TypeKind::Builtin(BuiltinType::Number),
        },
    );

    add!(
        types,
        "Text",
        BUILTIN_TYPES.text.id().unwrap(),
        ScopeValue::Type,
        Type {
            parameters: Vec::new(),
            kind: TypeKind::Builtin(BuiltinType::Text),
        },
    );
}
