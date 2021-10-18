mod assignment;
mod external;

pub use assignment::*;
pub use external::*;

use crate::lowering::*;
use wipple_parser::Intern;

impl Scope<'_> {
    pub fn root() -> Self {
        let mut scope = Scope::default();

        scope.insert(Intern::from(":"), builtin_assignment_operator);
        scope.insert(Intern::from("external"), builtin_external_function);

        scope
    }
}
