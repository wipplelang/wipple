use crate::*;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(TypeInfo, Debug, Clone)]
pub struct VariantSet {
    pub id: Id,
    pub variants: HashMap<String, VariantConstructor>,
}

#[derive(Debug, Clone)]
pub struct VariantConstructor {
    pub name: String,
    pub value_pattern: Option<Pattern>,
}

#[derive(TypeInfo, Debug, Clone)]
pub struct Variant {
    pub id: Id,
    pub name: String,
    pub value: Option<Value>,
}

pub(crate) fn setup(env: &mut EnvironmentInner) {
    env.set_variable(
        "variant",
        Value::of(Function::new(|value, env, stack| {
            let id = Id::new();

            let variants = if let Some(list) = value.get_if_present::<List>(env, stack)? {
                let mut variants = HashMap::new();

                for value in &list.items {
                    let name = value.get_or::<Name>("Expected name", env, stack)?.name;
                    variants.insert(
                        name.clone(),
                        VariantConstructor {
                            name,
                            value_pattern: None,
                        },
                    );
                }

                variants
            } else if let Some(block) = value.get_if_present::<Block>(env, stack)? {
                let variants = Rc::new(RefCell::new(Some(HashMap::new())));

                let variant_env: Environment = env::child_of(env).into();

                *variant_env.borrow_mut().assignment() = AssignmentFn::new({
                    let variants = variants.clone();
                    move |left, right, env, stack| {
                        let name = left
                            .into_value()
                            .get_or::<Name>("Expected name", env, stack)?
                            .name;

                        let right = right.evaluate(env, stack)?;

                        let value_pattern = if right.is_empty() {
                            None
                        } else {
                            Some(right.get_or::<Pattern>("Expected pattern", env, stack)?)
                        };

                        variants.borrow_mut().as_mut().unwrap().insert(
                            name.clone(),
                            VariantConstructor {
                                name,
                                value_pattern,
                            },
                        );

                        Ok(())
                    }
                });

                block.reduce(&variant_env, stack)?;

                variants.take().unwrap()
            } else {
                return Err(Return::error("Expected list or block of variants", stack));
            };

            Ok(Value::of(VariantSet { id, variants }))
        })),
    );

    // Variant-Set == Pattern
    env.add_primitive_relation(|variant_set: VariantSet| {
        Pattern::new(move |value, env, stack| {
            let variant = match value.get_if_present::<Variant>(env, stack)? {
                Some(value) => value,
                None => return Ok(Validated::Invalid),
            };

            Ok(if variant.id == variant_set.id {
                Validated::Valid(Value::of(variant))
            } else {
                Validated::Invalid
            })
        })
    });

    // Variant-Set == Module
    env.add_primitive_relation(|variant_set: VariantSet| {
        let mut env = env::blank();

        for (name, variant) in &variant_set.variants {
            env.set_variable(
                name,
                match variant.value_pattern {
                    Some(ref pattern) => {
                        let id = variant_set.id;
                        let name = variant.name.clone();
                        let pattern = pattern.clone();

                        Value::of(Function::new(move |value, env, stack| {
                            let value = value.evaluate(env, stack)?;
                            let validated = pattern(value, env, stack)?
                                .into_valid()
                                .ok_or_else(|| Return::error("Invalid value", stack))?;

                            Ok(Value::of(Variant {
                                id,
                                name: name.clone(),
                                value: Some(validated),
                            }))
                        }))
                    }
                    None => Value::of(Variant {
                        id: variant_set.id,
                        name: variant.name.clone(),
                        value: None,
                    }),
                },
            );
        }

        Module::new(env.into())
    });

    env.add_text_relation::<VariantSet>("variant set");

    // Variant == Pattern
    env.add_primitive_relation(|target: Variant| {
        Pattern::new(move |value, env, stack| {
            let variant = match value.get_if_present::<Variant>(env, stack)? {
                Some(value) => value,
                None => return Ok(Validated::Invalid),
            };

            Ok(if variant.id == target.id && variant.name == target.name {
                Validated::Valid(match variant.value {
                    Some(value) => value,
                    None => Value::of(variant),
                })
            } else {
                Validated::Invalid
            })
        })
    });

    env.add_relation(
        Trait::of::<Variant>(),
        Trait::of::<Text>(),
        DeriveValueFn::new(|value, env, stack| {
            let variant = value.into_primitive::<Variant>().unwrap();

            let text = match variant.value {
                Some(value) => {
                    let text = value.format(env, stack)?;
                    Text::new(&format!("{} {}", variant.name, text))
                }
                None => Text::new(&variant.name),
            };

            Ok(Value::of(text))
        }),
    );
}
