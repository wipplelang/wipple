use crate::{
    codegen::{CodegenCtx, CodegenResult, ir},
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
    ) -> CodegenResult<ir::DefinitionKey> {
        let key = ir::DefinitionKey {
            node: definition.clone(),
            substitutions: convert_substitutions(substitutions, db)?,
            bounds,
        };

        if !generic {
            self.insert_monomorphized_key(key.clone());
        }

        Ok(key)
    }

    pub fn insert_monomorphized_key(&mut self, key: ir::DefinitionKey) {
        self.definitions.insert(key);
    }
}

fn convert_substitutions(
    substitutions: &Substitutions,
    db: &Db,
) -> CodegenResult<BTreeMap<NodeRef, ir::Type>> {
    substitutions
        .entries()
        .map(|(parameter, ty)| {
            Ok((
                parameter,
                ty.key(db)
                    .ok_or_else(|| anyhow::format_err!("cannot codegen type"))?,
            ))
        })
        .collect()
}

impl MonomorphizeCtx {
    pub fn monomorphize_definitions(
        &mut self,
        db: &mut Db,
    ) -> CodegenResult<impl Iterator<Item = (ir::DefinitionKey, Vec<ir::Instruction>)> + use<>>
    {
        let mut cache = BTreeMap::new();

        for key in mem::take(&mut self.definitions) {
            self.monomorphize_definition(&key, &mut cache, db)?;
        }

        Ok(cache
            .into_iter()
            .map(|(key, instructions)| (key, instructions.unwrap())))
    }

    fn monomorphize_definition(
        &mut self,
        key: &ir::DefinitionKey,
        cache: &mut BTreeMap<ir::DefinitionKey, Option<Vec<ir::Instruction>>>,
        db: &mut Db,
    ) -> CodegenResult {
        if cache.contains_key(key) {
            return Ok(());
        }

        cache.insert(key.clone(), None);

        let Defined(definition) = db
            .get(&key.node)
            .ok_or_else(|| anyhow::format_err!("no definition for {key:?}"))?;

        let body = match definition {
            Definition::Constant(definition) => definition.value,
            Definition::Instance(definition) => definition.value,
            _ => None,
        }
        .ok_or_else(|| anyhow::format_err!("definition has no value"))?;

        let mut ctx = CodegenCtx::new(db);
        ctx.codegen(&body)?;
        ctx.instruction(ir::Instruction::Return {
            value: body.clone(),
        });

        let mut instructions = ctx.pop_instructions();
        for instruction in &mut instructions {
            instruction.traverse_mut(&mut |instruction| {
                if let ir::Instruction::Value { value, .. } = instruction {
                    match value {
                        ir::Value::Constant(constant_key) => {
                            self.monomorphize_key(
                                constant_key,
                                &key.substitutions,
                                &key.bounds,
                                cache,
                                db,
                            )?;
                        }
                        ir::Value::Bound(bound) => {
                            let Some(ir::Instance::Definition(mut resolved_key)) =
                                key.bounds.get(bound).cloned()
                            else {
                                return Err(anyhow::format_err!("bound {bound:?} not resolved"));
                            };

                            self.monomorphize_key(
                                &mut resolved_key,
                                &key.substitutions,
                                &key.bounds,
                                cache,
                                db,
                            )?;

                            *value = ir::Value::Constant(resolved_key);
                        }
                        _ => {}
                    }
                }

                Ok(())
            })?;
        }

        cache.get_mut(key).unwrap().replace(instructions);

        Ok(())
    }

    fn monomorphize_key(
        &mut self,
        key: &mut ir::DefinitionKey,
        substitutions: &BTreeMap<NodeRef, ir::Type>,
        bounds: &BTreeMap<NodeRef, ir::Instance>,
        cache: &mut BTreeMap<ir::DefinitionKey, Option<Vec<ir::Instruction>>>,
        db: &mut Db,
    ) -> CodegenResult {
        for ty in key.substitutions.values_mut() {
            self.monomorphize_type(ty, substitutions)?;
        }

        for instance in key.bounds.values_mut() {
            self.monomorphize_instance(instance, substitutions, bounds, cache, db)?;
        }

        self.monomorphize_definition(key, cache, db)?;

        Ok(())
    }

    fn monomorphize_type(
        &mut self,
        ty: &mut ir::Type,
        substitutions: &BTreeMap<NodeRef, ir::Type>,
    ) -> CodegenResult {
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

        if !success {
            return Err(anyhow::format_err!("could not monomorphize {ty:?}"))?;
        }

        Ok(())
    }

    fn monomorphize_instance(
        &mut self,
        instance: &mut ir::Instance,
        substitutions: &BTreeMap<NodeRef, ir::Type>,
        bounds: &BTreeMap<NodeRef, ir::Instance>,
        cache: &mut BTreeMap<ir::DefinitionKey, Option<Vec<ir::Instruction>>>,
        db: &mut Db,
    ) -> CodegenResult {
        match instance {
            ir::Instance::Bound(bound) => {
                *instance = bounds
                    .get(bound)
                    .ok_or_else(|| anyhow::format_err!("no bound for {bound:?}"))?
                    .clone();
            }
            ir::Instance::Definition(instance_key) => {
                self.monomorphize_key(instance_key, substitutions, bounds, cache, db)?;
            }
        }

        Ok(())
    }
}
