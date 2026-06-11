use crate::types::{ExtraType, MissingTypes, parse_atomic_type, visit_type};

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use wipple_core::{
    ast::AstKey,
    db::{Db, Node},
    span::{Span, Str},
    typecheck::{constraints::instantiate_constraint::InstantiateConstraint, ty::Ty},
    util::exact_for_each,
    visit::{Visit, Visitor, definitions::TypeDefinition},
};
use wipple_parse::{
    names::parse_type_name,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NamedType {
    pub span: Span,
    pub name: Str,
    pub parameters: Vec<AstKey>,
}

pub fn parse_named_type(parser: &mut Parser<'_>) -> Result<NamedType, ParseError> {
    let span = parser.spanned();
    let name = parse_type_name(parser)?;
    Ok(NamedType {
        span: span(parser),
        name,
        parameters: Vec::new(),
    })
}

pub fn parse_parameterized_type(parser: &mut Parser<'_>) -> Result<NamedType, ParseError> {
    let span = parser.spanned();
    let name = parse_type_name(parser)?;
    let parameters = parser.parse_many(1, parse_atomic_type)?;
    Ok(NamedType {
        span: span(parser),
        name,
        parameters,
    })
}

#[typetag::serde]
impl Visit for NamedType {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_type(db, node, visitor);

        let parameters = self
            .parameters
            .into_iter()
            .map(|parameter| visitor.visit(db, &parameter))
            .collect::<Vec<_>>();

        for &parameter in &parameters {
            db.graph.edge(parameter, node, "parameter");
        }

        let Some((type_definition_node, type_definition)) =
            visitor.resolve_as::<TypeDefinition>(db, &self.name, node)
        else {
            return;
        };

        let mut ty_parameters = BTreeMap::new();
        let (missing, extra) = exact_for_each(
            &type_definition.parameters,
            &parameters,
            |&parameter, &substitution| {
                ty_parameters.insert(parameter, Ty::Node(substitution));
            },
        );

        if !missing.is_empty() {
            db.insert(node, MissingTypes(missing.to_vec()));
        }

        for &parameter in extra {
            db.insert(parameter, ExtraType);
        }

        let substitutions = visitor.substitutions(
            BTreeMap::from([(type_definition_node, node)]),
            ty_parameters,
        );

        visitor.constraint(
            db,
            InstantiateConstraint {
                source_node: node,
                definition: type_definition_node,
                substitutions,
                traces: Vec::new(),
            },
        );
    }
}
