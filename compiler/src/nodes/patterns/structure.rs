use crate::{
    codegen::{Codegen, CodegenCtx, ir},
    database::{Node, NodeRef},
    nodes::{HasTemporaries, InheritTemporaries, Matching, parse_pattern, visit_pattern},
    syntax::{ParseError, Parser, TokenKind, parse_type_name, parse_variable_name},
    typecheck::{Instantation, InstantiateConstraint, Replacements, Substitutions},
    visit::{Definition, MatchPathSegment, Visit, Visitor},
};

#[derive(Debug)]
pub struct StructurePatternNode {
    pub name: String,
    pub fields: Vec<StructurePatternField>,
}

#[derive(Debug)]
pub struct StructurePatternField {
    pub name: String,
    pub pattern: NodeRef,
}

impl Node for StructurePatternNode {}

pub fn parse_structure_pattern(
    parser: &mut Parser<'_>,
) -> Result<StructurePatternNode, ParseError> {
    let name = parse_type_name(parser)?;
    parser.token(TokenKind::LeftBrace)?;

    let fields = parser
        .parse_lines(1, true, parse_structure_pattern_field)?
        .into_iter()
        .collect();

    parser.token(TokenKind::RightBrace)?;

    Ok(StructurePatternNode { name, fields })
}

pub fn parse_structure_pattern_field(
    parser: &mut Parser<'_>,
) -> Result<StructurePatternField, ParseError> {
    let name = parse_variable_name(parser)?;
    parser.token(TokenKind::AssignOperator)?;
    parser.consume_line_breaks();
    let pattern = parse_pattern(parser)?;

    Ok(StructurePatternField { name, pattern })
}

impl Visit for StructurePatternNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_pattern(node, visitor, None);

        let Some(definition) = visitor.resolve(&self.name, node, |definition| match definition {
            Definition::StructureConstructor(definition) => Some(definition.clone()),
            _ => None,
        }) else {
            return;
        };

        let mut field_temporaries = Vec::new();
        for field in &self.fields {
            let temporary = visitor.visit_matching(
                &field.pattern,
                definition
                    .fields
                    .get(&field.name)
                    .cloned()
                    .map(MatchPathSegment::Field),
            );

            visitor.edge(&field.pattern, node, "field");

            field_temporaries.push((field.name.as_str(), temporary));
        }

        let definition_node = definition.node.clone();
        let fields = definition.fields;

        let replacements = Replacements::from_iter([(definition_node.clone(), node.clone())]);
        for (field_name, field_type) in fields {
            if let Some((_, temporary)) = field_temporaries
                .iter()
                .find(|&(name, _)| *name == field_name)
            {
                replacements.insert(field_type, temporary.clone());
            }
        }

        visitor.constraint(InstantiateConstraint::new(Instantation {
            source_node: node.clone(),
            definition: definition_node,
            substitutions: Substitutions::new(),
            replacements,
            from_expression: true,
        }));

        visitor.insert(
            node,
            HasTemporaries(
                field_temporaries
                    .into_iter()
                    .map(|(_, temporary)| temporary)
                    .collect(),
            ),
        );

        visitor.insert(
            node,
            InheritTemporaries(
                self.fields
                    .iter()
                    .map(|field| field.pattern.clone())
                    .collect(),
            ),
        );
    }
}

impl Codegen for StructurePatternNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> Option<ir::SpannedExpression> {
        let HasTemporaries(field_temporaries) = ctx.get::<HasTemporaries>(node)?;
        let Matching(matching) = ctx.get(node)?;

        let mut expressions = Vec::new();
        for (field, temporary) in self.fields.iter().zip(field_temporaries) {
            expressions.push(
                ir::Expression::AssignTo(
                    Box::new(
                        ir::Expression::Field(
                            Box::new(ir::Expression::Variable(matching.clone()).at(node, ctx)),
                            field.name.clone(),
                        )
                        .at(node, ctx),
                    ),
                    temporary,
                )
                .at(node, ctx),
            );

            expressions.push(ctx.codegen(&field.pattern)?);
        }

        Some(ir::Expression::And(expressions).at(node, ctx))
    }
}
