//! Shared utilities for the compiler.

pub use web_sys;

#[macro_use]
mod log;

mod info;
pub use info::*;

use serde::{Deserialize, Serialize};
use ts_rs::TS;

/// Represented as `any` in TypeScript.
#[derive(
    Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, TS,
)]
#[ts(rename = "any")]
pub struct TsAny;
