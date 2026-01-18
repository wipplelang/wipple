use crate::{
    database::{Db, Fact, HiddenNode, NodeRef, Render},
    typecheck::{Replacements, Substitutions, Type, Typed},
};

#[derive(Debug, Clone)]
pub struct Instantiated {
    pub definition: NodeRef,
    pub from: NodeRef,
    pub source_node: NodeRef,
}

impl Fact for Instantiated {}

impl Render for Instantiated {}

pub struct InstantiateContext<'db> {
    pub db: &'db mut Db,
    pub definition: NodeRef,
    pub source_node: NodeRef,
    pub replacements: Replacements,
    pub substitutions: Substitutions,
}

impl InstantiateContext<'_> {
    pub fn instantiate_node(&mut self, node: &NodeRef) -> NodeRef {
        self.replacements.get_or_insert_with(node, || {
            let span = self.db.span(node);
            let instantiated = self.db.node(span, HiddenNode(None));

            self.db.insert(&instantiated, Typed::default());

            self.db.insert(
                &instantiated,
                Instantiated {
                    definition: self.definition.clone(),
                    from: node.clone(),
                    source_node: self.source_node.clone(),
                },
            );

            self.db.graph.replace(node, &instantiated);

            instantiated
        })
    }

    pub fn instantiate_type(&mut self, ty: &Type) -> Type {
        ty.traverse(|ty| match ty {
            Type::Node(node) => Type::Node(self.instantiate_node(&node)),
            Type::Constructed(ty) => {
                let Some(parameter) = &ty.instantiate else {
                    return Type::Constructed(ty);
                };

                self.substitutions.get_or_insert_with(parameter, || {
                    let span = self.db.span(parameter);
                    let substitution = self.db.node(span, HiddenNode(None));

                    self.db.insert(&substitution, Typed::default());

                    self.db.insert(
                        &substitution,
                        Instantiated {
                            definition: self.definition.clone(),
                            from: parameter.clone(),
                            source_node: self.source_node.clone(),
                        },
                    );

                    Type::Node(substitution)
                })
            }
        })
    }

    pub fn instantiate_substitutions(&mut self, substitutions: &Substitutions) -> Substitutions {
        substitutions
            .entries()
            .map(|(parameter, substitution)| (parameter, self.instantiate_type(&substitution)))
            .collect()
    }
}
