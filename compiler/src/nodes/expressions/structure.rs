use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Db, Fact, Node, NodeRef, Render},
    nodes::{parse_expression, visit_expression},
    syntax::{ParseError, Parser, TokenKind, parse_type_name, parse_variable_name},
    typecheck::{Instantation, InstantiateConstraint, Replacements, Substitutions},
    visit::{Definition, Visit, Visitor},
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct MissingField(pub String);

impl Fact for MissingField {}

impl Render for MissingField {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is missing field")
    }
}

#[derive(Debug, Clone)]
pub struct ExtraField(pub String);

impl Fact for ExtraField {}

impl Render for ExtraField {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is extra field")
    }
}

#[derive(Debug, Clone)]
pub struct DuplicateField;

impl Fact for DuplicateField {}

impl Render for DuplicateField {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is duplicate field")
    }
}

#[derive(Debug)]
pub struct StructureExpressionNode {
    pub name: String,
    pub fields: Vec<StructureExpressionField>,
}

#[derive(Debug)]
pub struct StructureExpressionField {
    pub name: String,
    pub value: NodeRef,
}

impl Node for StructureExpressionNode {}

pub fn parse_structure_expression(
    parser: &mut Parser<'_>,
) -> Result<StructureExpressionNode, ParseError> {
    let name = parse_type_name(parser)?;

    parser.token(TokenKind::LeftBrace)?;
    parser.commit("in this structure");

    let fields = parse_structure_expression_fields(parser)?;

    parser.token(TokenKind::RightBrace)?;

    Ok(StructureExpressionNode { name, fields })
}

pub fn parse_structure_expression_field(
    parser: &mut Parser<'_>,
) -> Result<StructureExpressionField, ParseError> {
    let name = parse_variable_name(parser)?;
    parser.token(TokenKind::AssignOperator)?;
    parser.consume_line_breaks();
    let value = parse_expression(parser)?;

    Ok(StructureExpressionField { name, value })
}

pub fn parse_structure_expression_fields(
    parser: &mut Parser<'_>,
) -> Result<Vec<StructureExpressionField>, ParseError> {
    parser.parse_lines(1, true, parse_structure_expression_field)
}

impl Visit for StructureExpressionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_expression(node, visitor);

        let mut fields = HashMap::new();
        for field in &self.fields {
            visitor.visit(&field.value);
            visitor.edge(&field.value, node, "field");
            fields.insert(field.name.clone(), field.value.clone());
        }

        let Some(definition) = visitor.resolve(&self.name, node, |definition| match definition {
            Definition::StructureConstructor(definition) => Some(definition.clone()),
            _ => None,
        }) else {
            return;
        };

        let mut field_values = HashMap::new();
        let replacements = Replacements::from_iter([(definition.node.clone(), node.clone())]);

        for (name, field) in definition.fields {
            if let Some(value) = fields.get(&name) {
                if field_values.insert(name.clone(), field.clone()).is_some() {
                    visitor.insert(node, DuplicateField);
                    continue;
                }

                replacements.insert(field, value.clone());
            } else {
                visitor.insert(node, MissingField(name));
            }
        }

        for name in fields.keys() {
            if !field_values.contains_key(name) {
                visitor.insert(node, ExtraField(name.clone()));
            }
        }

        visitor.constraint(InstantiateConstraint::new(Instantation {
            source_node: node.clone(),
            definition: definition.node,
            substitutions: Substitutions::new(),
            replacements,
        }));
    }
}

impl Codegen for StructureExpressionNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        ctx.write_string("{");
        for field in &self.fields {
            ctx.write_string(serde_json::to_string(&field.name).unwrap());
            ctx.write_string(": ");
            ctx.write(&field.value)?;
            ctx.write_string(", ");
        }
        ctx.write_string("}");

        Ok(())
    }
}
