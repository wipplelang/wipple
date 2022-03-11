use super::*;
use crate::compile::typecheck::BUILTIN_TYPES;

pub(super) fn load_builtins(scope: &Scope, info: &mut Info) {
    let mut scope_values = scope.values.borrow_mut();

    macro_rules! add {
        ($decls:ident, $name:expr, $id:expr, $value:expr) => {{
            let name = InternedString::new($name);
            let id = $id;

            info.declarations.$decls.insert(
                id,
                Declaration::Builtin(DeclarationKind {
                    name,
                    span: Span::new(FilePath::_Builtin, 0..0),
                    value: $value,
                }),
            );

            scope_values.insert(name, ScopeValue::Type(id));
        }};
    }

    add!(
        types,
        "Number",
        BUILTIN_TYPES.number.id().unwrap(),
        Type {
            parameters: Vec::new(),
            kind: TypeKind::Builtin(BuiltinType::Number),
        }
    );

    add!(
        types,
        "Text",
        BUILTIN_TYPES.text.id().unwrap(),
        Type {
            parameters: Vec::new(),
            kind: TypeKind::Builtin(BuiltinType::Text),
        }
    );
}
