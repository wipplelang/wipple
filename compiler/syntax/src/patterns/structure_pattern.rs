use crate::patterns::{Matching, Temporaries, parse_pattern, visit_pattern};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use wipple_core::{
    anyhow,
    ast::AstKey,
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    span::{Span, Str},
    typecheck::constraints::instantiate_constraint::InstantiateConstraint,
    visit::{
        Visit, Visitor, definitions::StructureConstructorDefinition,
        exhaustiveness::MatchPathSegment,
    },
};
use wipple_parse::{
    lexer::TokenKind,
    names::{parse_type_name, parse_variable_name},
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructurePattern {
    pub span: Span,
    pub name: Str,
    pub fields: Vec<StructurePatternField>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructurePatternField {
    pub span: Span,
    pub name: Str,
    pub pattern: AstKey,
}

pub fn parse_structure_pattern(parser: &mut Parser<'_>) -> Result<StructurePattern, ParseError> {
    let span = parser.spanned();
    let name = parse_type_name(parser)?;
    parser.token(TokenKind::LeftBrace)?;
    let fields = parser.parse_lines(1, true, parse_structure_pattern_field)?;
    parser.token(TokenKind::RightBrace)?;
    Ok(StructurePattern {
        span: span(parser),
        name,
        fields,
    })
}

pub fn parse_structure_pattern_field(
    parser: &mut Parser<'_>,
) -> Result<StructurePatternField, ParseError> {
    let span = parser.spanned();
    let name = parse_variable_name(parser)?;
    parser.token(TokenKind::AssignOperator)?;
    parser.consume_line_breaks();
    let pattern = parse_pattern(parser)?;
    Ok(StructurePatternField {
        span: span(parser),
        name,
        pattern,
    })
}

#[typetag::serde]
impl Visit for StructurePatternField {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }
}

#[typetag::serde]
impl Visit for StructurePattern {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_pattern(db, node, visitor, None);

        let Some((definition_node, definition)) =
            visitor.resolve_as::<StructureConstructorDefinition>(db, &self.name, node)
        else {
            return;
        };

        let mut field_patterns = Vec::new();
        for field in &self.fields {
            let segment = definition
                .fields
                .iter()
                .find(|(name, _)| name == &field.name)
                .map(|(_, ty)| MatchPathSegment::Field(*ty));

            let (pattern, temporary) = visitor.visit_matching(db, &field.pattern.clone(), segment);
            db.graph.edge(pattern, node, "field");

            field_patterns.push((field_patterns.len(), field.name.clone(), pattern, temporary));
        }

        let field_values = field_patterns
            .iter()
            .map(|(_, name, pattern, temporary)| {
                let value_index = definition
                    .fields
                    .iter()
                    .position(|(field_name, _)| field_name == name);
                (value_index, name.clone(), *pattern, *temporary)
            })
            .collect::<Vec<_>>();

        let mut nodes = BTreeMap::from([(definition_node, node)]);
        for (field_name, field_type) in &definition.fields {
            if let Some((_, _, _, temporary)) = field_patterns
                .iter()
                .find(|(_, name, _, _)| name == field_name)
            {
                nodes.insert(*field_type, *temporary);
            }
        }

        db.insert(
            node,
            Temporaries(
                field_patterns
                    .iter()
                    .map(|(_, _, _, temporary)| *temporary)
                    .collect::<BTreeSet<_>>(),
            ),
        );

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
            StructurePatternCodegen {
                node,
                fields: field_values,
            },
        );
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct StructurePatternCodegen {
    node: Node,
    fields: Vec<(Option<usize>, Str, Node, Node)>,
}

#[typetag::serde]
impl CodegenValue for StructurePatternCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        let matching = db
            .get::<Matching>(self.node)
            .ok_or_else(|| anyhow::format_err!("unresolved"))?
            .0;

        for (index, name, pattern, temporary) in &self.fields {
            let index = index.ok_or_else(|| anyhow::format_err!("unresolved"))?;

            ctx.condition(ir::Condition::Initialize {
                variable: *temporary,
                node: None,
                value: ir::Value::Field {
                    input: matching,
                    field_name: name.to_string(),
                    field_index: index,
                },
                mutable: false,
            });

            ctx.codegen(db, *pattern)?;
        }

        Ok(())
    }
}
