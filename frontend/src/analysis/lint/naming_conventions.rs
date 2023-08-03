use crate::{analysis, diagnostics::*, Compiler};
use convert_case::{Case, Casing};

impl Compiler {
    pub(super) fn naming_conventions_lint(&self, program: &analysis::Program) {
        // Syntax definitions are not checked here because they often use symbols

        for ty in program.declarations.types.values() {
            let name = ty.name.as_str();
            let correct = name.to_case(Case::Train);

            if name != correct {
                self.add_diagnostic(
                    self.warning(
                        "type should be written in title case with dashes",
                        vec![Note::primary(
                            ty.span,
                            format!("try writing the type like this: `{correct}`"),
                        )],
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
            let correct = name.to_case(Case::Train);

            if name != correct {
                self.add_diagnostic(
                    self.warning(
                        "trait should be written in title case with dashes",
                        vec![Note::primary(
                            tr.span,
                            format!("try writing the trait like this: `{correct}`"),
                        )],
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
                let correct = name.to_case(Case::Train);

                if name != correct {
                    self.add_diagnostic(
                        self.warning(
                            "pattern should be written in title case with dashes",
                            vec![Note::primary(
                                constant.span,
                                format!("try writing the pattern like this: `{correct}`"),
                            )],
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
                let correct = name.to_case(Case::Kebab);

                if name != correct {
                    self.add_diagnostic(
                        self.warning(
                            "constant should be written in lowercase with dashes",
                            vec![Note::primary(
                                constant.span,
                                format!("try writing the constant like this: `{correct}`"),
                            )],
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
                let correct = name.to_case(Case::Train);

                if name != correct {
                    self.add_diagnostic(
                        self.warning(
                            "type parameter should be written in title case with dashes",
                            vec![Note::primary(
                                param.span,
                                format!("try writing the type parameter like this: `{correct}`"),
                            )],
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
                let correct = name.to_case(Case::Kebab);

                if name != correct {
                    self.add_diagnostic(
                        self.warning(
                            "variable should be written in lowercase with dashes",
                            vec![Note::primary(
                                variable.span,
                                format!("try writing the variable like this: `{correct}`"),
                            )],
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
