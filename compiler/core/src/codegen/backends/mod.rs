pub mod js;

use crate::codegen::{CodegenError, mir};

pub trait Backend {
    type Output;

    fn run(self, program: &mir::Program) -> Result<Self::Output, CodegenError>;
}
