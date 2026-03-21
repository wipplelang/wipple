use crate::{
    codegen::{Codegen, CodegenCtx, CodegenResult, ir},
    database::{Fact, Node, NodeRef, Render},
    nodes::{Matching, Temporaries, parse_pattern, visit_pattern},
    syntax::{ParseError, Parser, TokenKind, parse_type_name, parse_variable_name},
    typecheck::{Instantation, InstantiateConstraint, Replacements, Substitutions},
    visit::{Definition, MatchPathSegment, Visit, Visitor},
};

#[derive(Debug)]
pub struct StructurePatternNode {
    pub name: String,
    pub fields: Vec<StructurePatternField>,
}

#[derive(Debug, Clone)]
pub struct StructurePatternField {
    pub name: String,
    pub pattern: NodeRef,
}

#[derive(Debug, Clone)]
struct ResolvedStructurePattern {
    fields: Vec<(Option<usize>, String, NodeRef)>,
}

impl Fact for ResolvedStructurePattern {}

impl Render for ResolvedStructurePattern {}

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
        let mut field_values = Vec::new();
        for field in &self.fields {
            let temporary = visitor.visit_matching(
                &field.pattern,
                definition
                    .fields
                    .iter()
                    .find_map(|(name, ty)| (*name == field.name).then_some(ty))
                    .cloned()
                    .map(MatchPathSegment::Field),
            );

            visitor.edge(&field.pattern, node, "field");

            field_temporaries.push((field.name.as_str(), temporary));
            field_values.push(field.clone());
        }

        let definition_node = definition.node.clone();

        let field_values = field_values
            .iter()
            .map(|field| {
                let index = definition
                    .fields
                    .iter()
                    .position(|(name, _)| *name == field.name);

                (index, field.name.clone(), field.pattern.clone())
            })
            .collect::<Vec<_>>();

        let replacements = Replacements::from_iter([(definition_node.clone(), node.clone())]);
        for (field_name, field_type) in &definition.fields {
            if let Some((_, temporary)) = field_temporaries
                .iter()
                .find(|&(name, _)| *name == *field_name)
            {
                replacements.insert(field_type.clone(), temporary.clone());
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
            ResolvedStructurePattern {
                fields: field_values,
            },
        );

        visitor.insert(
            node,
            Temporaries {
                has: field_temporaries
                    .into_iter()
                    .map(|(_, temporary)| temporary)
                    .collect(),
                inherit: self
                    .fields
                    .iter()
                    .map(|field| field.pattern.clone())
                    .collect(),
            },
        )
    }
}

impl Codegen for StructurePatternNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> CodegenResult {
        let Temporaries {
            has: field_temporaries,
            ..
        } = ctx
            .get(node)
            .ok_or_else(|| anyhow::format_err!("unresolved"))?;

        let Matching(matching) = ctx
            .get(node)
            .ok_or_else(|| anyhow::format_err!("unresolved"))?;

        let ResolvedStructurePattern { fields } = ctx
            .get(node)
            .ok_or_else(|| anyhow::format_err!("unresolved"))?;

        for ((index, name, pattern), temporary) in fields.iter().zip(field_temporaries) {
            let index = index.ok_or_else(|| anyhow::format_err!("unresolved"))?;

            ctx.condition(ir::Condition::Initialize {
                variable: temporary,
                node: None,
                value: ir::Value::Field {
                    input: matching.clone(),
                    field_name: name.clone(),
                    field_index: index,
                },
                mutable: false,
            });

            ctx.codegen(pattern)?;
        }

        Ok(())
    }
}
