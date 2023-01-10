macro_rules! lints {
    ($($lint:ident),* $(,)?) => {
        $(
            mod $lint;
        )*

        paste::paste! {
            impl Compiler<'_> {
                pub fn lint(&self, program: &analysis::Program) {
                    $(
                        self.[<$lint _lint>](program);
                    )*
                }
            }
        }
    };
}

use crate::{analysis, Compiler};

lints!(
    unused_unnecessary_type_parameter,
    unused_variable,
    useless_bounds,
);
