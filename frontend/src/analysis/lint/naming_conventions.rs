use crate::{analysis, diagnostics::*, Compiler};
use itertools::Itertools;

impl Compiler {
    pub(super) fn naming_conventions_lint(&self, program: &analysis::Program) {
        // Syntax definitions are not checked here because they often use symbols

        fn convert_case(s: &str, capitalize: bool) -> String {
            let boundaries = [
                convert_case::Boundary::Acronym,
                convert_case::Boundary::Hyphen,
                convert_case::Boundary::LowerUpper,
            ];

            convert_case::split(s, &boundaries)
                .into_iter()
                .map(|segment| {
                    if capitalize {
                        if segment.to_uppercase() == segment {
                            segment
                        } else {
                            let (head, tail) = segment.split_at(1);
                            head.to_uppercase() + tail
                        }
                    } else {
                        segment.to_lowercase()
                    }
                })
                .join("-")
        }

        for ty in program.declarations.types.values() {
            let name = ty.name.as_str();
            let correct = convert_case(name, true);

            if name != correct {
                self.add_diagnostic(
                    self.warning(
                        ty.span,
                        format!("type should be written as `{correct}`"),
                        "naming-conventions",
                    )
                    .fix(Fix::new(
                        "fix type name",
                        FixRange::replace(ty.span.first()),
                        correct,
                    )),
                );
            }
        }

        for tr in program.declarations.traits.values() {
            let name = tr.name.as_str();
            let correct = convert_case(name, true);

            if name != correct {
                self.add_diagnostic(
                    self.warning(
                        tr.span,
                        format!("trait should be written as `{correct}`"),
                        "naming-conventions",
                    )
                    .fix(Fix::new(
                        "fix trait name",
                        FixRange::replace(tr.span.first()),
                        correct,
                    )),
                );
            }
        }

        for constant in program.declarations.constants.values() {
            let name = constant.name.as_str();

            if constant.is_variant {
                let correct = convert_case(name, true);

                if name != correct {
                    self.add_diagnostic(
                        self.warning(
                            constant.span,
                            format!("pattern should be written as `{correct}`"),
                            "naming-conventions",
                        )
                        .fix(Fix::new(
                            "fix pattern name",
                            FixRange::replace(constant.span.first()),
                            correct,
                        )),
                    );
                }
            } else {
                let correct = convert_case(name, false);

                if name != correct {
                    self.add_diagnostic(
                        self.warning(
                            constant.span,
                            format!("constant should be written as `{correct}`"),
                            "naming-conventions",
                        )
                        .fix(Fix::new(
                            "fix constant name",
                            FixRange::replace(constant.span.first()),
                            correct,
                        )),
                    );
                }
            }
        }

        for param in program.declarations.type_parameters.values() {
            if let Some(name) = param.name {
                let name = name.as_str();
                let correct = convert_case(name, true);

                if name != correct {
                    self.add_diagnostic(
                        self.warning(
                            param.span,
                            format!("type parameter should be written as `{correct}`"),
                            "naming-conventions",
                        )
                        .fix(Fix::new(
                            "fix type parameter name",
                            FixRange::replace(param.span.first()),
                            correct,
                        )),
                    );
                }
            }
        }

        for variable in program.declarations.variables.values() {
            if let Some(name) = variable.name {
                let name = name.as_str();
                let correct = convert_case(name, false);

                if name != correct {
                    self.add_diagnostic(
                        self.warning(
                            variable.span,
                            format!("variable should be written as `{correct}`"),
                            "naming-conventions",
                        )
                        .fix(Fix::new(
                            "fix variable name",
                            FixRange::replace(variable.span.first()),
                            correct,
                        )),
                    );
                }
            }
        }
    }
}
