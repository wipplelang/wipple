use crate::typecheck::Info;

pub mod unused;

pub fn all(info: &mut Info) {
    unused::unused_variables(info);
}
