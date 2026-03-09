use crate::{
    codegen::{CodegenCtx, ir},
    database::{Db, NodeRef},
    typecheck::Substitutions,
    visit::{Defined, Definition},
};
use std::{
    collections::{BTreeMap, BTreeSet},
    mem,
};

#[derive(Debug, Default)]
pub struct MonomorphizeCtx {
    definitions: BTreeSet<ir::DefinitionKey>,
}

impl MonomorphizeCtx {
    pub fn get_or_insert(
        &mut self,
        definition: &NodeRef,
        substitutions: &Substitutions,
        bounds: BTreeMap<NodeRef, ir::Instance>,
        generic: bool,
        db: &Db,
    ) -> Option<ir::DefinitionKey> {
        let key = ir::DefinitionKey {
            node: definition.clone(),
            substitutions: convert_substitutions(substitutions, db)?,
            bounds,
        };

        if !generic {
            self.insert_monomorphized_key(key.clone());
        }

        Some(key)
    }

    pub fn insert_monomorphized_key(&mut self, key: ir::DefinitionKey) {
        self.definitions.insert(key);
    }
}

fn convert_substitutions(
    substitutions: &Substitutions,
    db: &Db,
) -> Option<BTreeMap<NodeRef, ir::Type>> {
    substitutions
        .entries()
        .map(|(parameter, ty)| Some((parameter, ty.key(db)?)))
        .collect::<Option<_>>()
}

impl CodegenCtx<'_> {
    #[must_use]
    pub fn monomorphize_definitions(
        &mut self,
    ) -> Option<impl Iterator<Item = (ir::DefinitionKey, ir::SpannedExpression)> + use<>> {
        let mut cache = BTreeMap::new();

        for key in mem::take(&mut self.monomorphize_ctx.definitions) {
            self.monomorphize_definition(&key, &mut cache)?;
        }

        Some(cache.into_iter().map(|(key, body)| (key, body.unwrap())))
    }

    #[must_use]
    fn monomorphize_definition(
        &mut self,
        key: &ir::DefinitionKey,
        cache: &mut BTreeMap<ir::DefinitionKey, Option<ir::SpannedExpression>>,
    ) -> Option<()> {
        if cache.contains_key(key) {
            return Some(());
        }

        cache.insert(key.clone(), None);

        let Defined(definition) = self.get(&key.node)?;

        let body = match definition {
            Definition::Constant(definition) => definition.value?,
            Definition::Instance(definition) => definition.value?,
            _ => return None,
        };

        let mut expression = self.codegen(&body)?;

        let mut success = true;
        expression.traverse_mut(&mut |expression| {
            if !success {
                return;
            }

            success = (|| {
                if let Some(ty) = &mut expression.ty {
                    self.monomorphize_type(ty, &key.substitutions)?;
                };

                match &mut expression.inner {
                    ir::Expression::Constant(constant_key) => {
                        self.monomorphize_key(
                            constant_key,
                            &key.substitutions,
                            &key.bounds,
                            cache,
                        )?;
                    }
                    ir::Expression::Bound(bound) => {
                        let ir::Instance::Definition(mut resolved) = key.bounds.get(bound)?.clone()
                        else {
                            return None;
                        };

                        self.monomorphize_key(
                            &mut resolved,
                            &key.substitutions,
                            &key.bounds,
                            cache,
                        )?;

                        expression.inner = ir::Expression::Constant(resolved);
                    }
                    _ => {}
                }

                Some(())
            })()
            .is_some();
        });

        if !success {
            return None;
        }

        cache.get_mut(key).unwrap().replace(expression);

        Some(())
    }

    #[must_use]
    fn monomorphize_key(
        &mut self,
        key: &mut ir::DefinitionKey,
        substitutions: &BTreeMap<NodeRef, ir::Type>,
        bounds: &BTreeMap<NodeRef, ir::Instance>,
        cache: &mut BTreeMap<ir::DefinitionKey, Option<ir::SpannedExpression>>,
    ) -> Option<()> {
        for ty in key.substitutions.values_mut() {
            self.monomorphize_type(ty, substitutions)?;
        }

        for instance in key.bounds.values_mut() {
            self.monomorphize_instance(instance, substitutions, bounds, cache)?;
        }

        self.monomorphize_definition(key, cache)?;

        Some(())
    }

    #[must_use]
    fn monomorphize_type(
        &mut self,
        ty: &mut ir::Type,
        substitutions: &BTreeMap<NodeRef, ir::Type>,
    ) -> Option<()> {
        let mut success = true;
        ty.traverse_mut(&mut |ty| {
            if !success {
                return;
            }

            if let ir::Type::Parameter(ref parameter) = *ty {
                if let Some(substitution) = substitutions.get(parameter) {
                    *ty = substitution.clone();
                } else {
                    success = false;
                }
            }
        });

        success.then_some(())
    }

    #[must_use]
    fn monomorphize_instance(
        &mut self,
        instance: &mut ir::Instance,
        substitutions: &BTreeMap<NodeRef, ir::Type>,
        bounds: &BTreeMap<NodeRef, ir::Instance>,
        cache: &mut BTreeMap<ir::DefinitionKey, Option<ir::SpannedExpression>>,
    ) -> Option<()> {
        match instance {
            ir::Instance::Bound(bound) => {
                *instance = bounds.get(bound)?.clone();
            }
            ir::Instance::Definition(instance_key) => {
                self.monomorphize_key(instance_key, substitutions, bounds, cache)?;
            }
        }

        Some(())
    }
}
