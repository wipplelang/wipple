use serde::{Deserialize, Serialize};
use std::{borrow::Cow, path::Path};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Constant<'a> {
    Number(f64),
    Text(Cow<'a, str>),
    Import(Cow<'a, Path>),
    // TODO
}
