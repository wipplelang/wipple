use crate::{analysis, diagnostics::*, Compiler};
use std::collections::BTreeMap;

impl Compiler<'_> {
    pub fn constant_unused_unnecessary_type_parameter_lint(&self, program: &analysis::Program) {
        for constant in program.declarations.constants.values() {
            let mut param_count = BTreeMap::<_, usize>::new();
            for param in constant.ty.params() {
                *param_count.entry(param).or_default() += 1;
            }

            for bound in &constant.bounds {
                for ty in &bound.params {
                    for param in ty.params() {
                        *param_count.entry(param).or_default() += 1;
                    }
                }
            }

            for &(span, param) in &constant.params {
                if param_count.get(&param).copied().unwrap_or(0) == 0 {
                    self.diagnostics.add(Diagnostic::warning(
                        "unused type parameter",
                        vec![Note::primary(span, "try removing this")],
                    ));
                }
            }

            for (&param, &count) in &param_count {
                if count == 1 {
                    if let Some(decl_span) = constant
                        .params
                        .iter()
                        .find_map(|&(span, p)| (p == param).then_some(span))
                    {
                        if let Some(use_span) = constant
                            .ty_annotation
                            .span_of(&analysis::TypeAnnotationKind::Parameter(param))
                        {
                            self.diagnostics.add(Diagnostic::warning(
                                "unnecessary type parameter",
                                vec![
                                    Note::primary(decl_span, "try removing this..."),
                                    Note::primary(use_span, "...and replacing this with `_`"),
                                ],
                            ));
                        }
                    }
                }
            }
        }
    }
}
