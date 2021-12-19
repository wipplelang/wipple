use serde::{Deserialize, Serialize};
use wipple_frontend::typecheck::*;

#[derive(Serialize, Deserialize)]
pub struct Bundle {
    pub item: Item,
}
