use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Db, Fact, Node, NodeRef, Render},
    nodes::{StringAttributeValueNode, parse_attribute_value},
    syntax::{ParseError, Parser, TokenKind, parse_attribute_name},
    visit::{Visit, Visitor},
};

#[derive(Debug, Clone)]
pub struct ExtraAttributeValue;

impl Fact for ExtraAttributeValue {}

impl Render for ExtraAttributeValue {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "extra attribute value")
    }
}

#[derive(Debug, Clone)]
pub struct DuplicateAttributeValue;

impl Fact for DuplicateAttributeValue {}

impl Render for DuplicateAttributeValue {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "duplicate attribute")
    }
}

#[derive(Debug, Clone)]
pub struct MismatchedAttributeValue;

impl Fact for MismatchedAttributeValue {}

impl Render for MismatchedAttributeValue {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "mismatched attribute value")
    }
}

#[derive(Debug, Clone)]
pub struct MissingAttributeValue;

impl Fact for MissingAttributeValue {}

impl Render for MissingAttributeValue {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "missing attribute value")
    }
}

pub fn parse_attributes(parser: &mut Parser<'_>) -> Result<Vec<NodeRef>, ParseError> {
    parser.parse_lines(0, false, parse_attribute)
}

#[derive(Debug)]
pub struct AttributeNode {
    pub name: String,
    pub value: Option<NodeRef>,
}

impl Node for AttributeNode {
    fn is_hidden(&self) -> bool {
        true
    }
}

pub fn parse_attribute(parser: &mut Parser<'_>) -> Result<NodeRef, ParseError> {
    parser.node(|parser| {
        parser.token(TokenKind::LeftBracket)?;

        let name = parse_attribute_name(parser)?;

        let value = parser.parse_optional(|parser| {
            parser.token(TokenKind::AssignOperator)?;
            parser.consume_line_breaks();
            parse_attribute_value(parser)
        })?;

        parser.token(TokenKind::RightBracket)?;

        Ok(AttributeNode { name, value })
    })
}

impl Db {
    pub fn contains_name_attribute(&mut self, attributes: &[NodeRef], name: &str) -> bool {
        let mut found = false;
        for node in attributes {
            let Some(attribute) = node.downcast_ref::<AttributeNode>() else {
                continue;
            };

            if attribute.name == name {
                if attribute.value.is_some() {
                    self.insert(node, ExtraAttributeValue);
                } else if found {
                    self.insert(node, DuplicateAttributeValue);
                } else {
                    found = true;
                }
            }
        }

        found
    }

    pub fn contains_assignment_attribute<T>(
        &mut self,
        attributes: &[NodeRef],
        name: &str,
        f: impl Fn(&NodeRef) -> Option<T>,
    ) -> Option<T> {
        let mut result = None;
        for node in attributes {
            let Some(attribute) = node.downcast_ref::<AttributeNode>() else {
                continue;
            };

            if attribute.name == name {
                if let Some(value) = &attribute.value {
                    if result.is_some() {
                        self.insert(node, DuplicateAttributeValue);
                        continue;
                    }

                    result = f(value);

                    if result.is_none() {
                        self.insert(node, MismatchedAttributeValue);
                    }
                } else {
                    self.insert(node, MissingAttributeValue);
                }
            }
        }

        result
    }

    pub fn contains_string_attribute_value(
        &mut self,
        attributes: &[NodeRef],
        name: &str,
    ) -> Option<String> {
        self.contains_assignment_attribute(attributes, name, |node| {
            node.downcast_ref::<StringAttributeValueNode>()
                .map(|node| node.value.clone())
        })
    }
}

impl Visit for AttributeNode {
    fn visit(&self, _node: &NodeRef, _visitor: &mut Visitor<'_>) {}
}

impl Codegen for AttributeNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        Err(ctx.error())
    }
}
