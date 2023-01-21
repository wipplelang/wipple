use super::{
    Expression, MonomorphizedExpression, MonomorphizedPattern, MonomorphizedPatternKind, Pattern,
    PatternKind, UnresolvedExpression, UnresolvedPatternKind,
};

macro_rules! traverse_expr_impl {
    ($prefix:literal, $f:expr, $expr:expr, $context:expr, $traverse:ident, &$($mut:tt)?) => {{
        paste::paste! {
            use super::[<$prefix ExpressionKind>]::*;

            let f = $f;
            let expr = $expr;
            let mut context = $context;

            f(expr, &mut context);

            match &$($mut)? expr.kind {
                Block(statements, _) => {
                    for statement in statements {
                        statement.$traverse(context.clone(), f);
                    }
                }
                End(value) => {
                    value.$traverse(context.clone(), f);
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
                _ => {}
            }
        }
    }};
}

macro_rules! traverse_expr {
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
                    traverse_expr_impl!($prefix, f, self, context, traverse_inner, &)
                }

                pub fn traverse_mut(&mut self, mut f: impl FnMut(&mut Self)) {
                    self.traverse_mut_with((), |expr, ()| f(expr));
                }

                pub fn traverse_mut_with<T: Clone>(&mut self, context: T, mut f: impl FnMut(&mut Self, &mut T)) {
                    self.traverse_mut_inner(context, &mut f);
                }

                fn traverse_mut_inner<T: Clone>(&mut self, context: T, f: &mut impl FnMut(&mut Self, &mut T)) {
                    traverse_expr_impl!($prefix, f, self, context, traverse_mut_inner, &mut)
                }
            }
        }
    };
}

traverse_expr!("Unresolved");
traverse_expr!("Monomorphized");
traverse_expr!("");

macro_rules! traverse_pattern_impl {
    ($prefix:literal, $f:expr, $pattern:expr, $context:expr, $traverse:ident, &$($mut:tt)?) => {{
        paste::paste! {
            use super::[<$prefix PatternKind>]::*;

            let f = $f;
            let pattern = $pattern;
            let mut context = $context;

            f(pattern, &mut context);

            match &$($mut)? pattern.kind {
                Or(left, right) => {
                    left.$traverse(context.clone(), f);
                    right.$traverse(context.clone(), f);
                }
                Where(pattern, _) => {
                    pattern.$traverse(context.clone(), f);
                }
                Tuple(patterns) => {
                    for pattern in patterns {
                        pattern.$traverse(context.clone(), f);
                    }
                }
                Destructure(fields) => {
                    for (_, pattern) in fields {
                        pattern.$traverse(context.clone(), f);
                    }
                }
                Variant(_, patterns) => {
                    for pattern in patterns {
                        pattern.$traverse(context.clone(), f);
                    }
                }
                _ => {}
            }
        }
    }};
}

macro_rules! traverse_pattern {
    ($prefix:literal) => {
        paste::paste! {
            #[allow(unused)]
            impl [<$prefix Pattern>] {
                pub fn traverse(&self, mut f: impl FnMut(&Self)) {
                    self.traverse_with((), |expr, ()| f(expr));
                }

                pub fn traverse_with<T: Clone>(&self, context: T, mut f: impl FnMut(&Self, &mut T)) {
                    self.traverse_inner(context, &mut f);
                }

                fn traverse_inner<T: Clone>(&self, context: T, f: &mut impl FnMut(&Self, &mut T)) {
                    traverse_pattern_impl!($prefix, f, self, context, traverse_inner, &)
                }

                pub fn traverse_mut(&mut self, mut f: impl FnMut(&mut Self)) {
                    self.traverse_mut_with((), |expr, ()| f(expr));
                }

                pub fn traverse_mut_with<T: Clone>(&mut self, context: T, mut f: impl FnMut(&mut Self, &mut T)) {
                    self.traverse_mut_inner(context, &mut f);
                }

                fn traverse_mut_inner<T: Clone>(&mut self, context: T, f: &mut impl FnMut(&mut Self, &mut T)) {
                    traverse_pattern_impl!($prefix, f, self, context, traverse_mut_inner, &mut)
                }
            }
        }
    };
}

traverse_pattern!("Monomorphized");
traverse_pattern!("");
