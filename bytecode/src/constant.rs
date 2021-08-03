use serde::{Deserialize, Serialize};
use std::borrow::Cow;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Constant<'a> {
    Number(f64),
    Text(Cow<'a, str>),
    // TODO
}
