use crate::{diagnostics::Diagnostics, lowering::*, parser::Span};
use internment::Intern;
use std::{collections::HashMap, sync::Arc};

#[derive(Default)]
pub struct Scope<'a> {
    pub parent: Option<&'a Scope<'a>>,
    variables: HashMap<Intern<String>, Arc<dyn Fn(Span, &mut Diagnostics) -> LoweredExpr>>,
}

impl<'a> Scope<'a> {
    pub fn root() -> Self {
        let mut scope = Scope::default();

        scope.insert(Intern::from(":"), builtin_assignment_operator);

        scope.insert(Intern::from("n"), |span, _| {
            LoweredExpr::new(
                span,
                LoweredExprKind::Constant(LoweredConstantExpr::new(
                    LoweredConstantExprKind::Number(Intern::new("42".parse().unwrap())),
                )),
            )
        });

        scope
    }

    pub fn child(&'a self) -> Self {
        Scope {
            parent: Some(self),
            ..Default::default()
        }
    }

    pub fn insert(
        &mut self,
        name: Intern<String>,
        variable: impl Fn(Span, &mut Diagnostics) -> LoweredExpr + 'static,
    ) {
        self.variables.insert(name, Arc::new(variable));
    }

    pub fn resolve(
        &self,
        name_expr: &NameExpr,
        diagnostics: &mut Diagnostics,
    ) -> Option<LoweredExpr> {
        self.variables
            .get(&name_expr.value)
            .map(|f| f(name_expr.span, diagnostics))
            .or_else(|| {
                self.parent.and_then(|p| {
                    p.resolve(name_expr, diagnostics)
                        .map(|expr| match &expr.kind {
                            LoweredExprKind::RuntimeVariable(runtime_variable_expr) => {
                                LoweredExpr::new(
                                    expr.span,
                                    LoweredExprKind::RuntimeVariable(
                                        runtime_variable_expr.clone().in_ancestor_scope(),
                                    ),
                                )
                            }
                            _ => expr,
                        })
                })
            })
    }
}
