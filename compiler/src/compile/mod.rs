pub mod ast;
pub mod build;
pub mod expand;
pub mod lower;
pub mod typecheck;

pub use typecheck::{Arm, Expression, ExpressionKind, Pattern, PatternKind, Program, Type};

pub type Declarations = typecheck::Declarations<Expression, Type>;
