use crate::lowering::*;
use std::{collections::HashMap, sync::Arc};
use wipple_diagnostics::*;
use wipple_parser::Intern;

#[derive(Default)]
pub struct Scope<'a> {
    pub parent: Option<&'a Scope<'a>>,
    variables: HashMap<Intern<String>, Arc<dyn Fn(Span, &mut Diagnostics) -> LoweredExpr>>,
}

impl<'a> Scope<'a> {
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
            .or_else(|| self.parent.and_then(|p| p.resolve(name_expr, diagnostics)))
    }
}
