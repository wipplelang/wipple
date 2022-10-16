use super::{
    Expression, MonomorphizedExpression, MonomorphizedPatternKind, PatternKind,
    UnresolvedExpression, UnresolvedPatternKind,
};

macro_rules! traverse_impl {
    ($prefix:literal, $f:expr, $expr:expr, $traverse:ident, &$($mut:tt)?) => {{
        paste::paste! {
            use super::[<$prefix ExpressionKind>]::*;

            let f = $f;
            let expr = $expr;

            f(expr);

            match &$($mut)? expr.kind {
                Block(statements) => {
                    for statement in statements {
                        statement.$traverse(f);
                    }
                }
                Call(function, input) => {
                    function.$traverse(f);
                    input.$traverse(f);
                }
                Function(_, body, _) => body.$traverse(f),
                When(input, arms) => {
                    input.$traverse(f);

                    for arm in arms {
                        if let [<$prefix PatternKind>]::Where(_, condition) = &$($mut)? arm.pattern.kind {
                            condition.$traverse(f);
                        }

                        arm.body.$traverse(f);
                    }
                },
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
                Variant(_, values) => {
                    for value in values {
                        value.$traverse(f);
                    }
                }
                Tuple(values) => {
                    for value in values {
                        value.$traverse(f);
                    }
                }
                _ => {}
            }
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
                    traverse_impl!($prefix, f, self, traverse_inner, &)
                }

                pub fn traverse_mut(&mut self, mut f: impl FnMut(&mut Self)) {
                    self.traverse_mut_inner(&mut f);
                }

                fn traverse_mut_inner(&mut self, f: &mut impl FnMut(&mut Self)) {
                    traverse_impl!($prefix, f, self, traverse_mut_inner, &mut)
                }
            }
        }
    };
}

traverse!("Unresolved");
traverse!("Monomorphized");
traverse!("");
