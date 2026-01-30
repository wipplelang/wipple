use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Node, NodeRef},
    nodes::{ExtraType, MissingTypes, parse_atomic_type, visit_constraint},
    syntax::{ParseError, Parser, TokenKind, parse_type_name},
    typecheck::{Bound, BoundConstraint, Substitutions},
    visit::{Definition, Visit, Visitor},
};

#[derive(Debug, Clone)]
pub struct BoundConstraintNode {
    pub trait_name: String,
    pub parameters: Vec<NodeRef>,
}

impl Node for BoundConstraintNode {}

pub fn parse_bound_constraint(parser: &mut Parser<'_>) -> Result<BoundConstraintNode, ParseError> {
    parser.token_with_reason(TokenKind::LeftParenthesis, "between these parentheses")?;

    let trait_name = parse_type_name(parser)?;

    let parameters = parser
        .parse_many(0, parse_atomic_type, |parser| parser.parse_nothing())?
        .into_iter()
        .map(|(node, _)| node)
        .collect();

    parser.token(TokenKind::RightParenthesis)?;

    Ok(BoundConstraintNode {
        trait_name,
        parameters,
    })
}

impl Visit for BoundConstraintNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_constraint(node, visitor);

        let Some(trait_definition) =
            visitor.resolve(&self.trait_name, node, |definition| match definition {
                Definition::Trait(definition) => Some(definition.clone()),
                _ => None,
            })
        else {
            return;
        };

        for parameter in &self.parameters {
            visitor.visit(parameter);
        }

        let mut definition_parameters = trait_definition.parameters.iter().cloned();
        let mut parameters = self.parameters.iter().cloned();

        let substitutions = definition_parameters
            .by_ref()
            .zip(parameters.by_ref())
            .map(|(parameter, substitution)| (parameter, substitution.into()))
            .collect::<Substitutions>();

        let missing = definition_parameters.collect::<Vec<_>>();
        let extra = parameters.collect::<Vec<_>>();

        if !missing.is_empty() {
            visitor.insert(node, MissingTypes(missing));
        }

        for node in extra {
            visitor.insert(&node, ExtraType);
        }

        visitor.constraint(BoundConstraint::new(
            node.clone(),
            Bound {
                source_node: node.clone(),
                bound_node: node.clone(),
                trait_node: trait_definition.node.clone(),
                substitutions,
                optional: false,
            },
        ));
    }
}

impl Codegen for BoundConstraintNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        Err(ctx.error())
    }
}
