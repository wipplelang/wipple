use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Node, NodeRef},
    nodes::{ExtraType, MissingTypes, parse_atomic_type, visit_type},
    syntax::{ParseError, Parser, parse_type_name},
    typecheck::{Instantation, InstantiateConstraint, Replacements, Substitutions},
    visit::{Definition, Visit, Visitor},
};

#[derive(Debug)]
pub struct NamedTypeNode {
    pub name: String,
    pub parameters: Vec<NodeRef>,
}

impl Node for NamedTypeNode {}

pub fn parse_named_type(parser: &mut Parser<'_>) -> Result<NamedTypeNode, ParseError> {
    let name = parse_type_name(parser)?;
    Ok(NamedTypeNode {
        name,
        parameters: Vec::new(),
    })
}

pub fn parse_parameterized_type(parser: &mut Parser<'_>) -> Result<NamedTypeNode, ParseError> {
    let name = parse_type_name(parser)?;

    let parameters = parser
        .parse_many(1, parse_atomic_type, |parser| parser.parse_nothing())?
        .into_iter()
        .map(|(node, _)| node)
        .collect();

    Ok(NamedTypeNode { name, parameters })
}

impl Visit for NamedTypeNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_type(node, visitor);

        for parameter in &self.parameters {
            visitor.visit(parameter);
            visitor.edge(parameter, node, "parameter");
        }

        let Some(type_definition) =
            visitor.resolve(&self.name, node, |definition| match definition {
                Definition::Type(definition) => Some(definition.clone()),
                _ => None,
            })
        else {
            return;
        };

        let mut definition_parameters = type_definition.parameters.iter().cloned();
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

        visitor.constraint(InstantiateConstraint::new(Instantation {
            source_node: node.clone(),
            definition: type_definition.node.clone(),
            substitutions,
            replacements: Replacements::from_iter([(type_definition.node, node.clone())]),

        }));
    }
}

impl Codegen for NamedTypeNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        Err(ctx.error())
    }
}
