use super::{
    Expression, MonomorphizedExpression, MonomorphizedPattern, Pattern, UnresolvedExpression,
};
use std::ops::ControlFlow;

macro_rules! traverse_expr_impl {
    ($prefix:literal, $enter:expr, $exit:expr, $expr:expr, $parent:expr, $context:expr, $pass_parent:expr, $traverse:ident, &$($mut:tt)?) => {{
        paste::paste! {
            use super::[<$prefix ExpressionKind>]::*;

            let enter = $enter;
            let exit = $exit;
            let expr = $expr;
            let parent = $parent;
            let mut context = $context;

            let should_traverse_children = (|| {
                enter(expr, parent, &mut context)?;

                match &$($mut)? expr.kind {
                    Block(statements, _) => {
                        for statement in statements {
                            statement.$traverse($pass_parent!(expr), context.clone(), enter, exit)?;
                        }
                    }
                    Call(function, input, _) => {
                        function.$traverse($pass_parent!(expr), context.clone(), enter, exit)?;
                        input.$traverse($pass_parent!(expr), context.clone(), enter, exit)?;
                    }
                    Function(_, body, _) => {
                        body.$traverse($pass_parent!(expr), context.clone(), enter, exit)?;
                    }
                    When(input, arms) => {
                        input.$traverse($pass_parent!(expr), context.clone(), enter, exit)?;

                        for arm in arms {
                            if let Some(guard) = &$($mut)? arm.guard {
                                guard.$traverse($pass_parent!(expr), context.clone(), enter, exit)?;
                            }

                            arm.body.$traverse($pass_parent!(expr), context.clone(), enter, exit)?;
                        }
                    },
                    Initialize(_, value) => {
                        value.$traverse($pass_parent!(expr), context.clone(), enter, exit)?;
                    }
                    External(_, _, inputs) => {
                        for input in inputs {
                            input.$traverse($pass_parent!(expr), context.clone(), enter, exit)?;
                        }
                    }
                    Intrinsic(_, inputs) => {
                        for input in inputs {
                            input.$traverse($pass_parent!(expr), context.clone(), enter, exit)?;
                        }
                    }
                    Plugin(_, _, inputs) => {
                        for input in inputs {
                            input.$traverse($pass_parent!(expr), context.clone(), enter, exit)?;
                        }
                    }
                    Structure(fields) => {
                        for field in fields {
                            field.$traverse($pass_parent!(expr), context.clone(), enter, exit)?;
                        }
                    }
                    Variant(_, values) => {
                        for value in values {
                            value.$traverse($pass_parent!(expr), context.clone(), enter, exit)?;
                        }
                    }
                    Tuple(values) => {
                        for value in values {
                            value.$traverse($pass_parent!(expr), context.clone(), enter, exit)?;
                        }
                    }
                    Format(segments, _) => {
                        for (_, segment) in segments {
                            segment.$traverse($pass_parent!(expr), context.clone(), enter, exit)?;
                        }
                    }
                    With((_, value), body) => {
                        value.$traverse($pass_parent!(expr), context.clone(), enter, exit)?;
                        body.$traverse($pass_parent!(expr), context.clone(), enter, exit)?;
                    }
                    End(value) => {
                        value.$traverse($pass_parent!(expr), context.clone(), enter, exit)?;
                    }
                    Semantics(_, expr) => {
                        expr.$traverse($pass_parent!(expr), context.clone(), enter, exit)?;
                    }
                    _ => {}
                }

                exit(expr, parent, &mut context)?;

                ControlFlow::Continue(())
            })();

            if matches!(should_traverse_children, ControlFlow::Continue(()) | ControlFlow::Break(true)) {
                ControlFlow::Continue(())
            } else {
                ControlFlow::Break(false)
            }
        }
    }};
}

macro_rules! traverse_expr {
    ($prefix:literal) => {
        paste::paste! {
            #[allow(unused)]
            impl [<$prefix Expression>] {
                pub fn traverse<'a>(
                    &'a self,
                    mut enter: impl FnMut(&'a Self) -> ControlFlow<bool, ()>,
                    mut exit: impl FnMut(&'a Self) -> ControlFlow<bool, ()>,
                ) {
                    self.traverse_with((), |expr, ()| enter(expr), |expr, ()| exit(expr));
                }

                pub fn traverse_with_parent<'a>(
                    &'a self,
                    mut enter: impl FnMut(&'a Self, Option<&'a Self>) -> ControlFlow<bool, ()>,
                    mut exit: impl FnMut(&'a Self, Option<&'a Self>) -> ControlFlow<bool, ()>,
                ) {
                    self.traverse_inner(
                        None,
                        (),
                        &mut |expr, parent, ()| enter(expr, parent),
                        &mut |expr, parent, ()| exit(expr, parent),
                    );
                }

                pub fn traverse_with<'a, T: Clone>(
                    &'a self,
                    context: T,
                    mut enter: impl FnMut(&'a Self, &mut T) -> ControlFlow<bool, ()>,
                    mut exit: impl FnMut(&'a Self, &mut T) -> ControlFlow<bool, ()>,
                ) {
                    self.traverse_inner(
                        None,
                        context,
                        &mut |expr, _, context| enter(expr, context),
                        &mut |expr, _, context| exit(expr, context),
                    );
                }

                fn traverse_inner<'a, T: Clone>(
                    &'a self,
                    parent: Option<&'a Self>,
                    context: T,
                    enter: &mut impl FnMut(&'a Self, Option<&'a Self>, &mut T) -> ControlFlow<bool, ()>,
                    exit: &mut impl FnMut(&'a Self, Option<&'a Self>, &mut T) -> ControlFlow<bool, ()>,
                ) -> ControlFlow<bool, ()> {
                    macro_rules! pass_through {
                        ($expr:expr) => {
                            Some($expr)
                        }
                    }

                    traverse_expr_impl!($prefix, enter, exit, self, parent, context, pass_through, traverse_inner, &)
                }

                pub fn traverse_mut(
                    &mut self,
                    mut enter: impl FnMut(&mut Self) -> ControlFlow<bool, ()>,
                    mut exit: impl FnMut(&mut Self) -> ControlFlow<bool, ()>,
                ) {
                    self.traverse_mut_with((), |expr, ()| enter(expr), |expr, ()| exit(expr));
                }

                pub fn traverse_mut_with<T: Clone>(
                    &mut self,
                    context: T,
                    mut enter: impl FnMut(&mut Self, &mut T) -> ControlFlow<bool, ()>,
                    mut exit: impl FnMut(&mut Self, &mut T) -> ControlFlow<bool, ()>,
                ) {
                    self.traverse_mut_inner(
                        None,
                        context,
                        &mut |expr, _, context| enter(expr, context),
                        &mut |expr, _, context| exit(expr, context),
                    );
                }

                fn traverse_mut_inner<T: Clone>(
                    &mut self,
                    parent: Option<()>,
                    context: T,
                    enter: &mut impl FnMut(&mut Self, Option<()>, &mut T) -> ControlFlow<bool, ()>,
                    exit: &mut impl FnMut(&mut Self, Option<()>, &mut T) -> ControlFlow<bool, ()>,
                ) -> ControlFlow<bool, ()> {
                    macro_rules! ignore {
                        ($expr:expr) => {
                            None
                        }
                    }

                    traverse_expr_impl!($prefix, enter, exit, self, None, context, ignore, traverse_mut_inner, &mut)
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
                Tuple(patterns) => {
                    for pattern in patterns {
                        pattern.$traverse(context.clone(), f);
                    }
                }
                Destructure(.., fields) => {
                    for (_, pattern) in fields {
                        pattern.$traverse(context.clone(), f);
                    }
                }
                Variant(.., patterns) => {
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
