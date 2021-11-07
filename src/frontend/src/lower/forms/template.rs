use crate::lower::*;
use std::{fmt, num::NonZeroUsize, sync::Arc};

#[derive(Clone, Serialize)]
pub struct Template {
    pub arity: Option<NonZeroUsize>,

    #[serde(skip)]
    #[allow(clippy::type_complexity)]
    pub expand: Arc<dyn Fn(LowerContext, Vec<Expr>, Span, &Stack, &mut Info) -> Option<Form>>,
}

impl Template {
    pub fn new(
        arity: Option<NonZeroUsize>,
        expand: impl Fn(LowerContext, Vec<Expr>, Span, &Stack, &mut Info) -> Option<Form> + 'static,
    ) -> Self {
        Template {
            arity,
            expand: Arc::new(expand),
        }
    }

    pub fn expand(
        &self,
        context: LowerContext,
        exprs: Vec<Expr>,
        span: Span,
        stack: &Stack,
        info: &mut Info,
    ) -> Option<Form> {
        if let Some(arity) = self.arity {
            assert_eq!(exprs.len(), arity.into());
        }

        (self.expand)(context, exprs, span, stack, info)
    }
}

impl fmt::Debug for Template {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Template")
            .field("arity", &self.arity)
            .finish()
    }
}
