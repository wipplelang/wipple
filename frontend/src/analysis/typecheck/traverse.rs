use super::{
    Expression, MonomorphizedExpression, MonomorphizedPatternKind, PatternKind,
    UnresolvedExpression, UnresolvedPatternKind,
};

macro_rules! traverse_impl {
    ($prefix:literal, $f:expr, $expr:expr, $context:expr, $traverse:ident, &$($mut:tt)?) => {{
        paste::paste! {
            use super::[<$prefix ExpressionKind>]::*;

            let f = $f;
            let expr = $expr;
            let mut context = $context;

            f(expr, &mut context);

            match &$($mut)? expr.kind {
                Block(statements) => {
                    for statement in statements {
                        statement.$traverse(context.clone(), f);
                    }
                }
                Call(function, input) => {
                    function.$traverse(context.clone(), f);
                    input.$traverse(context.clone(), f);
                }
                Function(_, body, _) => body.$traverse(context.clone(), f),
                When(input, arms) => {
                    input.$traverse(context.clone(), f);

                    for arm in arms {
                        if let [<$prefix PatternKind>]::Where(_, condition) = &$($mut)? arm.pattern.kind {
                            condition.$traverse(context.clone(), f);
                        }

                        arm.body.$traverse(context.clone(), f);
                    }
                },
                Initialize(_, value) => value.$traverse(context.clone(), f),
                External(_, _, inputs) => {
                    for input in inputs {
                        input.$traverse(context.clone(), f);
                    }
                }
                Runtime(_, inputs) => {
                    for input in inputs {
                        input.$traverse(context.clone(), f);
                    }
                }
                Structure(fields) => {
                    for field in fields {
                        field.$traverse(context.clone(), f);
                    }
                }
                Variant(_, values) => {
                    for value in values {
                        value.$traverse(context.clone(), f);
                    }
                }
                Tuple(values) => {
                    for value in values {
                        value.$traverse(context.clone(), f);
                    }
                }
                // Deliberately ignore the contents of a BlackBox expression
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
                    self.traverse_with((), |expr, ()| f(expr));
                }

                pub fn traverse_with<T: Clone>(&self, context: T, mut f: impl FnMut(&Self, &mut T)) {
                    self.traverse_inner(context, &mut f);
                }

                fn traverse_inner<T: Clone>(&self, context: T, f: &mut impl FnMut(&Self, &mut T)) {
                    traverse_impl!($prefix, f, self, context, traverse_inner, &)
                }

                pub fn traverse_mut(&mut self, mut f: impl FnMut(&mut Self)) {
                    self.traverse_mut_with((), |expr, ()| f(expr));
                }

                pub fn traverse_mut_with<T: Clone>(&mut self, context: T, mut f: impl FnMut(&mut Self, &mut T)) {
                    self.traverse_mut_inner(context, &mut f);
                }

                fn traverse_mut_inner<T: Clone>(&mut self, context: T, f: &mut impl FnMut(&mut Self, &mut T)) {
                    traverse_impl!($prefix, f, self, context, traverse_mut_inner, &mut)
                }
            }
        }
    };
}

traverse!("Unresolved");
traverse!("Monomorphized");
traverse!("");
