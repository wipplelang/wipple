use crate::lower::*;
use std::{
    cell::RefMut,
    ops::{Deref, DerefMut},
};

pub struct NameExpr {
    pub span: Span,
    pub value: LocalIntern<String>,
}

impl NameExpr {
    pub fn new(span: Span, value: LocalIntern<String>) -> Self {
        NameExpr { span, value }
    }
}

impl Expr for NameExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn lower_to_form(self, stack: &Stack, info: &mut Info) -> SpannedForm {
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

    fn lower_to_binding(self, _: &Stack, _: &mut Info) -> Option<SpannedBinding> {
        Some(SpannedBinding::from(NameBinding::new(
            self.span, self.value,
        )))
    }
}

impl NameExpr {
    pub(super) fn resolve(&self, mut stack: &Stack, info: &mut Info) -> Option<SpannedForm> {
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
                        stack = parent;
                    } else {
                        break None;
                    }
                };
            }

            if let Some(variable) = stack.variables.borrow().get(&self.value) {
                let form = (variable.form)(self.span);

                if matches!(form.form, Form::Item(_)) {
                    for mut list in used {
                        list.insert(variable.id);
                    }
                }

                break Some(form);
            } else {
                if let Some(captures) = &stack.captures {
                    used.push(EitherRefMut::Cell(captures.borrow_mut()));
                }

                parent!();
            }
        }
    }
}
