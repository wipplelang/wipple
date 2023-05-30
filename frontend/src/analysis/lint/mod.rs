use crate::{analysis, Compiler};

macro_rules! lints {
    ($($lint:ident),* $(,)?) => {
        $(
            mod $lint;
        )*

        paste::paste! {
            impl Compiler {
                pub(crate) fn lint(&self, program: &analysis::Program) {
                    $(
                        self.[<$lint _lint>](program);
                    )*
                }
            }
        }
    };
}

lints!(
    naming_conventions,
    unused_variable,
    useless_bounds,
    useless_expression,
);
