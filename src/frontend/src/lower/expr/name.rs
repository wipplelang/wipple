use crate::lower::*;
use std::{
    cell::RefMut,
    ops::{Deref, DerefMut},
};
use wipple_parser::Intern;

pub struct NameExpr {
    pub span: Span,
    pub value: Intern<String>,
}

impl NameExpr {
    pub fn new(span: Span, value: Intern<String>) -> Self {
        NameExpr { span, value }
    }
}

impl Expr for NameExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn lower_to_form(self, stack: Stack, info: &mut Info) -> SpannedForm {
        match self.resolve(stack, info) {
            Some(form) => form,
            None => {
                info.diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    format!("'{}' is not defined", self.value),
                    vec![Note::primary(
                        self.span,
                        "This name does not resolve to a variable",
                    )],
                ));

                SpannedForm::from(SpannedItem::error(self.span))
            }
        }
    }

    fn lower_to_binding(self, _: Stack, _: &mut Info) -> Option<SpannedBinding> {
        Some(SpannedBinding::from(NameBinding::new(
            self.span, self.value,
        )))
    }
}

impl NameExpr {
    pub(super) fn resolve(&self, mut stack: Stack, info: &mut Info) -> Option<SpannedForm> {
        enum EitherRefMut<'a, T> {
            Cell(RefMut<'a, T>),
            Ref(&'a mut T),
        }

        impl<'a, T> Deref for EitherRefMut<'a, T> {
            type Target = T;

            fn deref(&self) -> &Self::Target {
                match self {
                    EitherRefMut::Cell(r) => &*r,
                    EitherRefMut::Ref(r) => r,
                }
            }
        }

        impl<'a, T> DerefMut for EitherRefMut<'a, T> {
            fn deref_mut(&mut self) -> &mut Self::Target {
                match self {
                    EitherRefMut::Cell(r) => &mut *r,
                    EitherRefMut::Ref(r) => r,
                }
            }
        }

        let mut used = vec![EitherRefMut::Ref(&mut info.used_variables)];

        loop {
            macro_rules! parent {
                () => {
                    if let Some(parent) = stack.parent {
                        stack = *parent;
                    } else {
                        break None;
                    }
                };
            }

            match stack.scope {
                Scope::Function {
                    parameter_name,
                    parameter,
                    captures,
                } => {
                    if parameter_name == self.value {
                        break Some((parameter.form)(self.span));
                    } else {
                        parent!();
                        used.push(EitherRefMut::Cell(captures.borrow_mut()));
                    }
                }
                Scope::Block { variables } => {
                    if let Some(variable) = variables.borrow().get(&self.value) {
                        for mut list in used {
                            list.push(variable.id);
                        }

                        break Some((variable.form)(self.span));
                    } else {
                        parent!();
                    }
                }
            }
        }
    }
}
