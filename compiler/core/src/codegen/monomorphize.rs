use crate::{
    codegen::{CodegenCtx, CodegenError, ir, types::ir_type},
    db::{Db, Node},
    typecheck::ty::Ty,
    visit::definitions::{ConstantDefinition, ConstantValue, Defined, InstanceDefinition},
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
        db: &Db,
        definition: Node,
        parameters: &BTreeMap<Node, Ty>,
        bounds: BTreeMap<Node, ir::Instance>,
        generic: bool,
    ) -> Result<ir::DefinitionKey, CodegenError> {
        let key = ir::DefinitionKey {
            node: definition,
            substitutions: convert_substitutions(db, parameters)?,
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
    db: &Db,
    substitutions: &BTreeMap<Node, Ty>,
) -> Result<BTreeMap<Node, ir::Type>, CodegenError> {
    substitutions
        .iter()
        .map(|(&parameter, ty)| {
            Ok((
                parameter,
                ir_type(db, ty.clone())
                    .ok_or_else(|| anyhow::format_err!("cannot codegen type {ty:?}"))?,
            ))
        })
        .collect()
}

impl MonomorphizeCtx {
    pub fn monomorphize_definitions(
        &mut self,
        db: &Db,
    ) -> Result<impl Iterator<Item = (ir::DefinitionKey, ir::Definition)> + use<>, CodegenError>
    {
        let mut cache = BTreeMap::new();

        for key in mem::take(&mut self.definitions) {
            self.monomorphize_definition(db, &key, &mut cache)?;
        }

        Ok(cache
            .into_iter()
            .map(|(key, definition)| (key, definition.unwrap())))
    }

    fn monomorphize_definition(
        &mut self,
        db: &Db,
        key: &ir::DefinitionKey,
        cache: &mut BTreeMap<ir::DefinitionKey, Option<ir::Definition>>,
    ) -> Result<(), CodegenError> {
        if cache.contains_key(key) {
            return Ok(());
        }

        cache.insert(key.clone(), None);

        let Defined(definition) = db
            .get(key.node)
            .ok_or_else(|| anyhow::format_err!("no definition for {key:?}"))?;

        let body = if definition.downcast_ref::<ConstantDefinition>().is_some() {
            db.get(key.node).map(|ConstantValue(value)| *value)
        } else if let Some(definition) = definition.downcast_ref::<InstanceDefinition>() {
            definition.value
        } else {
            None
        }
        .ok_or_else(|| anyhow::format_err!("definition {key:?} has no value"))?;

        let mut ctx = CodegenCtx::new();
        ctx.codegen(db, body)?;
        ctx.instruction(ir::Instruction::Return { value: body });

        let mut instructions = ctx.pop_instructions();
        let mut types = BTreeMap::new();
        for instruction in &mut instructions {
            instruction.traverse_mut(&mut |instruction| {
                instruction.for_each_node(true, &mut |node| {
                    if let Some(mut ty) = ir_type(db, Ty::Node(*node)) {
                        ty.substitute(&key.substitutions);
                        types.insert(*node, ty);
                    }
                });

                if let ir::Instruction::Value { value, .. } = instruction {
                    match value {
                        ir::Value::Constant(constant_key) => {
                            self.monomorphize_key(
                                db,
                                constant_key,
                                &key.substitutions,
                                &key.bounds,
                                cache,
                            )?;
                        }
                        ir::Value::Bound(bound) => {
                            let Some(ir::Instance::Definition(mut resolved_key)) =
                                key.bounds.get(bound).cloned()
                            else {
                                return Err(anyhow::format_err!("bound {bound:?} not resolved"));
                            };

                            self.monomorphize_key(
                                db,
                                &mut resolved_key,
                                &key.substitutions,
                                &key.bounds,
                                cache,
                            )?;

                            *value = ir::Value::Constant(resolved_key);
                        }
                        _ => {}
                    }
                }

                Ok(())
            })?;
        }

        cache.get_mut(key).unwrap().replace(ir::Definition {
            ty: types.get(&body).cloned(),
            instructions,
            types,
            imports: Vec::new(), // will be populated via `collect_imports`
        });

        Ok(())
    }

    fn monomorphize_key(
        &mut self,
        db: &Db,
        key: &mut ir::DefinitionKey,
        substitutions: &BTreeMap<Node, ir::Type>,
        bounds: &BTreeMap<Node, ir::Instance>,
        cache: &mut BTreeMap<ir::DefinitionKey, Option<ir::Definition>>,
    ) -> Result<(), CodegenError> {
        for ty in key.substitutions.values_mut() {
            self.monomorphize_type(ty, substitutions)?;
        }

        for instance in key.bounds.values_mut() {
            self.monomorphize_instance(db, instance, substitutions, bounds, cache)?;
        }

        self.monomorphize_definition(db, key, cache)?;

        Ok(())
    }

    fn monomorphize_type(
        &mut self,
        ty: &mut ir::Type,
        substitutions: &BTreeMap<Node, ir::Type>,
    ) -> Result<(), CodegenError> {
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
        db: &Db,
        instance: &mut ir::Instance,
        substitutions: &BTreeMap<Node, ir::Type>,
        bounds: &BTreeMap<Node, ir::Instance>,
        cache: &mut BTreeMap<ir::DefinitionKey, Option<ir::Definition>>,
    ) -> Result<(), CodegenError> {
        match instance {
            ir::Instance::Bound(bound) => {
                *instance = bounds
                    .get(bound)
                    .ok_or_else(|| anyhow::format_err!("no bound for {bound:?}"))?
                    .clone();
            }
            ir::Instance::Definition(instance_key) => {
                self.monomorphize_key(db, instance_key, substitutions, bounds, cache)?;
            }
        }

        Ok(())
    }
}
