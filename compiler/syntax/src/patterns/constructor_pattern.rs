use crate::patterns::{ExtraElement, Matching, Temporaries, parse_atomic_pattern, visit_pattern};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use wipple_core::{
    anyhow,
    ast::AstKey,
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    span::{Span, Str},
    typecheck::{
        constraints::{instantiate_constraint::InstantiateConstraint, ty_constraint::TyConstraint},
        groups::Typed,
        ty::{ConstructedTy, Ty},
    },
    visit::{
        Visit, Visitor,
        definitions::{MarkerConstructorDefinition, VariantConstructorDefinition},
        exhaustiveness::MatchPathSegment,
    },
};
use wipple_parse::{
    names::parse_constructor_name,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConstructorPattern {
    pub span: Span,
    pub constructor: Str,
    pub elements: Vec<AstKey>,
}

pub fn parse_parameterized_constructor_pattern(
    parser: &mut Parser<'_>,
) -> Result<ConstructorPattern, ParseError> {
    let span = parser.spanned();
    let constructor = parse_constructor_name(parser)?;
    let elements = parser.parse_many(1, parse_atomic_pattern)?;
    Ok(ConstructorPattern {
        span: span(parser),
        constructor,
        elements,
    })
}

pub fn parse_constructor_pattern(
    parser: &mut Parser<'_>,
) -> Result<ConstructorPattern, ParseError> {
    let span = parser.spanned();
    let constructor = parse_constructor_name(parser)?;
    Ok(ConstructorPattern {
        span: span(parser),
        constructor,
        elements: Vec::new(),
    })
}

#[typetag::serde]
impl Visit for ConstructorPattern {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        #[derive(Debug)]
        enum ConstructorDefinition {
            Marker,
            Variant(VariantConstructorDefinition),
        }

        let definition = visitor.resolve_matching(db, &self.constructor, node, |_, definition| {
            if definition
                .downcast_ref::<MarkerConstructorDefinition>()
                .is_some()
            {
                return Some(ConstructorDefinition::Marker);
            }

            if let Some(definition) = definition.downcast_ref::<VariantConstructorDefinition>() {
                return Some(ConstructorDefinition::Variant(definition.clone()));
            }

            None
        });

        let Some((definition_node, definition)) = definition else {
            visit_pattern(db, node, visitor, None);
            return;
        };

        match definition {
            ConstructorDefinition::Marker => {
                visit_pattern(db, node, visitor, Some(MatchPathSegment::Match));

                for element in self.elements {
                    let (pattern, _) = visitor.visit_matching(db, &element, None);
                    db.graph.edge(pattern, node, "element");
                    db.insert(pattern, ExtraElement);
                }

                let substitutions = visitor.substitutions(
                    BTreeMap::from([(definition_node, node)]),
                    Default::default(),
                );

                visitor.constraint(
                    db,
                    InstantiateConstraint::new(node, definition_node, substitutions),
                );

                visitor.codegen(db, node, ConstructorPatternCodegen::Marker);
            }
            ConstructorDefinition::Variant(definition) => {
                let terminal = self
                    .elements
                    .is_empty()
                    .then_some(MatchPathSegment::Variant(definition_node));

                visit_pattern(db, node, visitor, terminal);

                let element_count = self.elements.len();
                let elements = self
                    .elements
                    .into_iter()
                    .enumerate()
                    .map(|(index, element)| {
                        let element = visitor.visit_matching(
                            db,
                            &element,
                            MatchPathSegment::VariantElement(definition_node, index, element_count),
                        );

                        db.graph.edge(element.0, node, "element");

                        element
                    })
                    .collect::<Vec<_>>();

                if element_count == 0 {
                    let substitutions = visitor.substitutions(
                        BTreeMap::from([(definition_node, node)]),
                        Default::default(),
                    );

                    visitor.constraint(
                        db,
                        InstantiateConstraint::new(node, definition_node, substitutions),
                    );
                } else {
                    let constructor_node = db.node();
                    db.insert(constructor_node, Typed::default());

                    let substitutions = visitor.substitutions(
                        BTreeMap::from([(definition_node, constructor_node)]),
                        Default::default(),
                    );

                    visitor.constraint(
                        db,
                        InstantiateConstraint::new(node, definition_node, substitutions),
                    );

                    visitor.constraint(
                        db,
                        TyConstraint::new(
                            constructor_node,
                            Ty::Constructed(ConstructedTy::function(
                                elements.iter().map(|(_, temporary)| *temporary).collect(),
                                node,
                            )),
                        ),
                    );
                }

                db.insert(
                    node,
                    Temporaries(
                        elements
                            .iter()
                            .map(|(_, temporary)| *temporary)
                            .collect::<BTreeSet<_>>(),
                    ),
                );

                visitor.codegen(
                    db,
                    node,
                    ConstructorPatternCodegen::Variant {
                        node,
                        index: definition.index,
                        elements,
                    },
                );
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
enum ConstructorPatternCodegen {
    Marker,
    Variant {
        node: Node,
        index: usize,
        elements: Vec<(Node, Node)>,
    },
}

#[typetag::serde]
impl CodegenValue for ConstructorPatternCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        match self {
            ConstructorPatternCodegen::Marker => Ok(()),
            ConstructorPatternCodegen::Variant {
                node,
                index,
                elements,
            } => {
                let matching = db
                    .get::<Matching>(*node)
                    .ok_or_else(|| anyhow::format_err!("unresolved"))?
                    .0;

                ctx.condition(ir::Condition::EqualToVariant {
                    input: matching,
                    variant: *index,
                });

                for (element_index, &(pattern, temporary)) in elements.iter().enumerate() {
                    ctx.condition(ir::Condition::Initialize {
                        variable: temporary,
                        node: None,
                        value: ir::Value::VariantElement {
                            input: matching,
                            variant: *index,
                            index: element_index,
                        },
                        mutable: false,
                    });

                    ctx.codegen(db, pattern)?;
                }

                Ok(())
            }
        }
    }
}
