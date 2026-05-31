use crate::{
    codegen::{CodegenCtx, CodegenError, ir},
    db::{Db, Node},
    visit::definitions::{ConstantDefinition, ConstantValue, Defined, InstanceDefinition},
};
use std::{
    collections::{BTreeMap, BTreeSet},
    mem,
};

#[derive(Debug, Default)]
pub struct MonomorphizeCtx {
    definitions: BTreeSet<ir::ConstantDefinitionKey>,
}

impl MonomorphizeCtx {
    pub fn get_or_insert(
        &mut self,
        definition: Node,
        bounds: BTreeMap<Node, ir::Instance>,
        generic: bool,
    ) -> Result<ir::ConstantDefinitionKey, CodegenError> {
        let key = ir::ConstantDefinitionKey {
            node: definition,
            bounds,
        };

        if !generic {
            self.insert_monomorphized_key(key.clone());
        }

        Ok(key)
    }

    pub fn insert_monomorphized_key(&mut self, key: ir::ConstantDefinitionKey) {
        self.definitions.insert(key);
    }
}

impl MonomorphizeCtx {
    pub fn monomorphize_definitions(
        &mut self,
        db: &Db,
    ) -> Result<impl Iterator<Item = (ir::ConstantDefinitionKey, ir::Function)> + use<>, CodegenError>
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
        key: &ir::ConstantDefinitionKey,
        cache: &mut BTreeMap<ir::ConstantDefinitionKey, Option<ir::Function>>,
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
        for instruction in &mut instructions {
            instruction.traverse_mut(&mut |instruction| {
                if let ir::Instruction::Value { value, .. } = instruction {
                    match value {
                        ir::Value::Constant(constant_key) => {
                            let ir::DefinitionKey::Constant(constant_key) = constant_key else {
                                return Err(anyhow::format_err!(
                                    "expected constant for definition key {constant_key:?}"
                                ))?;
                            };

                            self.monomorphize_key(db, constant_key, &key.bounds, cache)?;
                        }
                        ir::Value::Bound(bound) => {
                            let Some(ir::Instance::Definition(mut resolved_key)) =
                                key.bounds.get(bound).cloned()
                            else {
                                return Err(anyhow::format_err!("bound {bound:?} not resolved"));
                            };

                            self.monomorphize_key(db, &mut resolved_key, &key.bounds, cache)?;

                            *value = ir::Value::Constant(ir::DefinitionKey::Constant(resolved_key));
                        }
                        _ => {}
                    }
                }

                Ok(())
            })?;
        }

        cache.get_mut(key).unwrap().replace(ir::Function {
            instructions,
            ..Default::default()
        });

        Ok(())
    }

    fn monomorphize_key(
        &mut self,
        db: &Db,
        key: &mut ir::ConstantDefinitionKey,
        bounds: &BTreeMap<Node, ir::Instance>,
        cache: &mut BTreeMap<ir::ConstantDefinitionKey, Option<ir::Function>>,
    ) -> Result<(), CodegenError> {
        for instance in key.bounds.values_mut() {
            self.monomorphize_instance(db, instance, bounds, cache)?;
        }

        self.monomorphize_definition(db, key, cache)?;

        Ok(())
    }

    fn monomorphize_instance(
        &mut self,
        db: &Db,
        instance: &mut ir::Instance,
        bounds: &BTreeMap<Node, ir::Instance>,
        cache: &mut BTreeMap<ir::ConstantDefinitionKey, Option<ir::Function>>,
    ) -> Result<(), CodegenError> {
        match instance {
            ir::Instance::Bound(bound) => {
                *instance = bounds
                    .get(bound)
                    .ok_or_else(|| anyhow::format_err!("no bound for {bound:?}"))?
                    .clone();
            }
            ir::Instance::Definition(instance_key) => {
                self.monomorphize_key(db, instance_key, bounds, cache)?;
            }
        }

        Ok(())
    }
}
