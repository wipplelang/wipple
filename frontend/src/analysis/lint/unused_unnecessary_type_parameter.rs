use crate::{analysis, diagnostics::*, Compiler};
use std::collections::BTreeMap;

impl Compiler<'_> {
    pub(super) fn unused_unnecessary_type_parameter_lint(&self, program: &analysis::Program) {
        let add_diagnostic = |decl_span, use_span| {
            self.add_warning(
                "unnecessary type parameter",
                vec![
                    Note::primary(decl_span, "try removing this..."),
                    Note::primary(use_span, "...and replacing this with `_`"),
                ],
            )
        };

        macro_rules! lint {
            (@constant $decl:ident) => {
                lint!(
                    @lint $decl,
                    |_| {},
                    |decl_span, param| {
                        if let Some(use_span) = $decl
                            .ty_annotation
                            .span_of(&analysis::TypeAnnotationKind::Parameter(param))
                        {
                            add_diagnostic(decl_span, use_span);
                        }
                    }
                )
            };
            (@instance $decl:ident) => {
                lint!(
                    @lint $decl,
                    |param_count: &mut BTreeMap<_, _>| {
                        for ty in &$decl.trait_params {
                            for param in ty.params() {
                                *param_count.entry(param).or_default() += 1;
                            }
                        }
                    },
                    |_, _| {}
                )
            };
            (@lint $decl:ident, $count:expr, $extra:expr) => {{
                let mut param_count = BTreeMap::<_, usize>::new();
                for param in $decl.ty.params() {
                    *param_count.entry(param).or_default() += 1;
                }

                for bound in &$decl.bounds {
                    for ty in &bound.params {
                        for param in ty.params() {
                            *param_count.entry(param).or_default() += 1;
                        }
                    }
                }

                $count(&mut param_count);

                for &(span, param) in &$decl.params {
                    if param_count.get(&param).copied().unwrap_or(0) == 0 {
                        self.add_warning(
                            "unused type parameter",
                            vec![Note::primary(span, "try removing this")],
                        );
                    }
                }

                for (&param, &count) in &param_count {
                    if count == 1 {
                        if let Some(decl_span) = $decl
                            .params
                            .iter()
                            .find_map(|&(span, p)| (p == param).then_some(span))
                        {
                            if let Some(use_span) =
                                $decl.bound_annotations.iter().find_map(|(_, params)| {
                                    params.iter().find_map(|ty| {
                                        ty.span_of(&analysis::TypeAnnotationKind::Parameter(param))
                                    })
                                })
                            {
                                add_diagnostic(decl_span, use_span);
                            }

                            $extra(decl_span, param)
                        }
                    }
                }
            }};
        }

        for constant in program.declarations.constants.values() {
            lint!(@constant constant);
        }

        for instance in program
            .declarations
            .instances
            .values()
            .flat_map(|instances| instances.values())
        {
            lint!(@instance instance);
        }
    }
}
