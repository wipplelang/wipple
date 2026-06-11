use crate::expressions::{parse_expression, visit_expression};

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use wipple_core::{
    ast::AstKey,
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Fact, Node},
    render::Render,
    span::{Span, Str},
    typecheck::constraints::instantiate_constraint::InstantiateConstraint,
    visit::{Visit, Visitor, definitions::StructureConstructorDefinition},
};
use wipple_parse::{
    lexer::TokenKind,
    names::{parse_type_name, parse_variable_name},
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MissingField(pub String);

#[typetag::serde]
impl Fact for MissingField {}

impl Render for MissingField {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExtraField(pub String);

#[typetag::serde]
impl Fact for ExtraField {}

impl Render for ExtraField {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DuplicateField;

#[typetag::serde]
impl Fact for DuplicateField {}

impl Render for DuplicateField {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructureExpressionField {
    pub span: Span,
    pub name: Str,
    pub value: AstKey,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructureExpression {
    pub span: Span,
    pub name: Str,
    pub fields: Vec<StructureExpressionField>,
}

pub fn parse_structure_expression(
    parser: &mut Parser<'_>,
) -> Result<StructureExpression, ParseError> {
    let span = parser.spanned();
    let name = parse_type_name(parser)?;
    parser.token(TokenKind::LeftBrace)?;
    parser.commit("in this structure");
    let fields = parse_structure_expression_fields(parser)?;
    parser.token(TokenKind::RightBrace)?;
    Ok(StructureExpression {
        span: span(parser),
        name,
        fields,
    })
}

pub fn parse_structure_expression_field(
    parser: &mut Parser<'_>,
) -> Result<StructureExpressionField, ParseError> {
    let span = parser.spanned();
    let name = parse_variable_name(parser)?;
    parser.token(TokenKind::AssignOperator)?;
    parser.consume_line_breaks();
    let value = parse_expression(parser)?;
    Ok(StructureExpressionField {
        span: span(parser),
        name,
        value,
    })
}

pub fn parse_structure_expression_fields(
    parser: &mut Parser<'_>,
) -> Result<Vec<StructureExpressionField>, ParseError> {
    parser.parse_lines(1, true, parse_structure_expression_field)
}

#[typetag::serde]
impl Visit for StructureExpressionField {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }
}

#[typetag::serde]
impl Visit for StructureExpression {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_expression(db, node, visitor);

        let Some((definition_node, definition)) =
            visitor.resolve_as::<StructureConstructorDefinition>(db, &self.name, node)
        else {
            return;
        };

        let mut field_values = Vec::new();
        for field in self.fields {
            let value = visitor.visit(db, &field.value);
            db.graph.edge(value, node, "field");

            field_values.push((field_values.len(), field.name.clone(), value));
        }

        let mut nodes = BTreeMap::from([(definition_node, node)]);
        for (field_name, field_type) in &definition.fields {
            if let Some((_, _, value)) = field_values.iter().find(|(_, name, _)| name == field_name)
            {
                if nodes.contains_key(field_type) {
                    db.insert(node, DuplicateField);
                } else {
                    nodes.insert(*field_type, *value);
                }
            } else {
                db.insert(node, MissingField(field_name.to_string()));
            }
        }

        for (_, field_name, _) in &field_values {
            if !definition.fields.iter().any(|(name, _)| name == field_name) {
                db.insert(node, ExtraField(field_name.to_string()));
            }
        }

        let substitutions = visitor.substitutions(nodes, Default::default());

        visitor.constraint(
            db,
            InstantiateConstraint {
                source_node: node,
                definition: definition_node,
                substitutions,
                traces: Vec::new(),
            },
        );

        visitor.codegen(
            db,
            node,
            StructureExpressionCodegen {
                node,
                fields: field_values,
            },
        );
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct StructureExpressionCodegen {
    node: Node,
    fields: Vec<(usize, Str, Node)>,
}

#[typetag::serde]
impl CodegenValue for StructureExpressionCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        let mut fields = self.fields.clone();
        fields.sort_by_key(|(index, _, _)| *index);

        for (_, _, field) in &fields {
            ctx.codegen(db, *field)?;
        }

        ctx.instruction(ir::Instruction::Value {
            node: self.node,
            value: ir::Value::Structure(
                fields
                    .into_iter()
                    .map(|(_, name, value)| (name.to_string(), value))
                    .collect(),
            ),
        });

        Ok(())
    }
}
