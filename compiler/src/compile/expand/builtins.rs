use crate::{
    compile::expand::{
        Expander, Node, NodeKind, Operator, OperatorPrecedence, Scope, ScopeValue, Template,
        TemplateBody,
    },
    diagnostics::*,
    helpers::InternedString,
    parse::Span,
    FilePath, Loader,
};

pub(super) fn load_builtins<L: Loader>(expander: &mut Expander<L>, scope: &Scope) {
    let builtin_span = Span::new(FilePath::_Builtin, 0..0);
    let mut scope_values = scope.values.borrow_mut();

    // ':' operator

    let id = expander.compiler.new_template_id();

    scope_values.insert(
        InternedString::new(":"),
        ScopeValue::Operator(Operator {
            precedence: OperatorPrecedence::Assignment,
            template: id,
        }),
    );

    expander.info.templates.insert(
        id,
        Template::function(builtin_span, |expander, span, mut inputs, scope| {
            let rhs = inputs.pop().unwrap();
            let lhs = inputs.pop().unwrap();

            if let NodeKind::Template(inputs, body) = rhs.kind {
                let name = match lhs.kind {
                    NodeKind::Name(name) => name,
                    _ => todo!(),
                };

                let id = expander.compiler.new_template_id();

                let template = Template {
                    span: rhs.span,
                    body: TemplateBody::Syntax(inputs, *body),
                };

                expander.info.templates.insert(id, template);

                scope
                    .values
                    .borrow_mut()
                    .insert(name, ScopeValue::Template(id));

                Node {
                    span,
                    kind: NodeKind::Unit,
                }
            } else {
                Node {
                    span,
                    kind: NodeKind::Assign(Box::new(lhs), Box::new(rhs)),
                }
            }
        }),
    );

    // 'use' template

    let id = expander.compiler.new_template_id();

    scope_values.insert(InternedString::new("use"), ScopeValue::Template(id));

    expander.info.templates.insert(
        id,
        Template::function(builtin_span, |expander, span, mut inputs, scope| {
            if inputs.len() != 1 {
                expander.compiler.diagnostics.add(Diagnostic::error(
                    "expected 1 input to template 'use'",
                    vec![Note::primary(span, "try removing some of these inputs")],
                ));

                return Node {
                    span,
                    kind: NodeKind::Error,
                };
            }

            let input = inputs.pop().unwrap();

            let path = match input.kind {
                NodeKind::Text(text) => text,
                _ => {
                    expander.compiler.diagnostics.add(Diagnostic::error(
                        "expected text here",
                        vec![Note::primary(span, "'use' only accepts a file path or URL")],
                    ));

                    return Node {
                        span,
                        kind: NodeKind::Error,
                    };
                }
            };

            if let Some(imported_scope) =
                (expander.load)(expander.compiler, FilePath::Path(path), expander.info)
            {
                scope
                    .values
                    .borrow_mut()
                    .extend(imported_scope.values.clone().into_inner());
            }

            Node {
                span,
                kind: NodeKind::Unit,
            }
        }),
    );
}
