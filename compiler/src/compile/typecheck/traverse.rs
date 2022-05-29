use super::{
    Expression, ExpressionKind, MonomorphizedExpression, MonomorphizedExpressionKind, Program,
    UnresolvedExpression, UnresolvedExpressionKind,
};

impl Program {
    pub fn traverse(&self, mut f: impl FnMut(&Expression)) {
        for statement in &self.body {
            statement.traverse_inner(&mut f);
        }
    }

    pub fn traverse_mut(&mut self, mut f: impl FnMut(&mut Expression)) {
        for statement in &mut self.body {
            statement.traverse_mut_inner(&mut f);
        }
    }
}

macro_rules! traverse_impl {
    ($kind:ident, $f:expr, $expr:expr, $traverse:ident, &$($mut:tt)?) => {{
        use $kind::*;

        let f = $f;
        let expr = $expr;

        f(expr);

        match &$($mut)? expr.kind {
            Block(statements, _) => {
                for statement in statements {
                    statement.$traverse(f);
                }
            }
            Call(function, input) => {
                function.$traverse(f);
                input.$traverse(f);
            }
            Function(body, _) => body.$traverse(f),
            When(_, _) => todo!(),
            Initialize(_, value) => value.$traverse(f),
            External(_, _, inputs) => {
                for input in inputs {
                    input.$traverse(f);
                }
            }
            Structure(fields) => {
                for field in fields {
                    field.$traverse(f);
                }
            }
            ListLiteral(items) => {
                for item in items {
                    item.$traverse(f);
                }
            }
            _ => {}
        }
    }};
}

macro_rules! traverse {
    ($prefix:literal) => {
        paste::paste! {
            #[allow(unused)]
            impl [<$prefix Expression>] {
                pub fn traverse(&self, mut f: impl FnMut(&Self)) {
                    self.traverse_inner(&mut f);
                }

                fn traverse_inner(&self, f: &mut impl FnMut(&Self)) {
                    traverse_impl!([<$prefix ExpressionKind>], f, self, traverse_inner, &)
                }

                pub fn traverse_mut(&mut self, mut f: impl FnMut(&mut Self)) {
                    self.traverse_mut_inner(&mut f);
                }

                fn traverse_mut_inner(&mut self, f: &mut impl FnMut(&mut Self)) {
                    traverse_impl!([<$prefix ExpressionKind>], f, self, traverse_mut_inner, &mut)
                }
            }
        }
    };
}

traverse!("Unresolved");
traverse!("Monomorphized");
traverse!("");
