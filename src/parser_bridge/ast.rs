use crate::parser_bridge::Location;
use serde::{Deserialize, Serialize};

#[derive(Clone, Serialize, Deserialize)]
pub struct AST {
    #[serde(flatten)]
    node: ASTNode,
    location: Location,
}

#[derive(Clone, Serialize, Deserialize)]
pub enum ASTNode {
    Block(Vec<Vec<AST>>),
    List(Vec<AST>),
    Name(String),
    Text(String),
    Number(String),
    Quoted(Box<AST>),
}
