use crate::analysis::{self, typecheck::Typechecker};
use std::ops::ControlFlow;

impl Typechecker {
    pub fn check_access(&self, expr: &analysis::Expression) {
        expr.traverse(
            |expr| {
                match &expr.kind {
                    analysis::ExpressionKind::Function(pattern, _, _)
                    | analysis::ExpressionKind::Initialize(pattern, _) => {
                        pattern.traverse(|pattern| {
                            self.check_access_in_pattern(pattern);
                        });
                    }
                    analysis::ExpressionKind::When(_, arms) => {
                        for arm in arms {
                            arm.pattern.traverse(|pattern| {
                                self.check_access_in_pattern(pattern);
                            });
                        }
                    }
                    analysis::ExpressionKind::Structure(_) => {
                        let id = match expr.ty.kind {
                            analysis::TypeKind::Named(id, _, _) => id,
                            _ => return ControlFlow::Continue(()),
                        };

                        let (ty_decl_span, ty_is_sealed) = self
                            .with_type_decl(id, |decl| (decl.span, decl.attributes.sealed))
                            .unwrap();

                        if ty_is_sealed && expr.span.first().path != ty_decl_span.first().path {
                            self.compiler.add_error(
                                expr.span,
                                "cannot create a value of a sealed structure outside the file in which the structure is defined",
                                "",
                            );
                        }
                    }
                    analysis::ExpressionKind::Constant(item) => {
                        let constant = match self.items.borrow().get(item) {
                            Some((Some((_, constant)), _)) => *constant,
                            _ => return ControlFlow::Continue(()),
                        };

                        let id = match self.declarations.borrow().constants.get(&constant).and_then(|constant| constant.enumeration_ty) {
                            Some(ty) => ty,
                            None => return ControlFlow::Continue(()),
                        };

                        let (ty_decl_span, ty_is_sealed) = self
                            .with_type_decl(id, |decl| (decl.span, decl.attributes.sealed))
                            .unwrap();

                        if ty_is_sealed && expr.span.first().path != ty_decl_span.first().path {
                            self.compiler.add_error(
                                expr.span,
                                "cannot refer to a variant of a sealed enumeration outside the file in which the enumeration is defined",
                                "",
                            );
                        }
                    }
                    _ => {}
                }
                ControlFlow::Continue(())
            },
            |_| ControlFlow::Continue(()),
        );
    }

    fn check_access_in_pattern(&self, pattern: &analysis::Pattern) {
        match pattern.kind {
            analysis::PatternKind::Destructure(id, _) => {
                let (ty_decl_span, ty_is_sealed) = self
                    .with_type_decl(id, |decl| (decl.span, decl.attributes.sealed))
                    .unwrap();

                if ty_is_sealed && pattern.span.first().path != ty_decl_span.first().path {
                    self.compiler.add_error(
                        pattern.span,
                        "cannot destructure a sealed structure outside the file in which the structure is defined",
                        "",
                    );
                }
            }
            analysis::PatternKind::Variant(id, _, _) => {
                let (ty_decl_span, ty_is_sealed) = self
                    .with_type_decl(id, |decl| (decl.span, decl.attributes.sealed))
                    .unwrap();

                if ty_is_sealed && pattern.span.first().path != ty_decl_span.first().path {
                    self.compiler.add_error(
                        pattern.span,
                        "cannot match a variant of a sealed enumeration outside the file in which the enumeration is defined",
                        "",
                    );
                }
            }
            _ => {}
        }
    }
}
