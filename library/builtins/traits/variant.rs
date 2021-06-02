use crate::*;
use std::collections::HashMap;
use wipple::*;

#[derive(TypeInfo, Debug, Clone)]
pub struct VariantSet {
    pub id: Id,
    pub variants: HashMap<String, VariantConstructor>,
}

impl Primitive for VariantSet {}

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

impl Primitive for Variant {}

pub(crate) fn setup(env: &Env, stack: &Stack) -> Result<()> {
    env.set_variable(
        stack,
        "variant",
        Value::of(Function::new(|value, env, stack| {
            let id = Id::new();

            let variants = if let Some(list) = value.get_if_present::<List>(env, stack)? {
                let mut variants = HashMap::new();

                for value in &list.items {
                    let name = value
                        .get_or::<Name>("Expected name", env, stack)?
                        .name
                        .clone();

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
                let variants: Rc<RefCell<HashMap<String, VariantConstructor>>> = Default::default();

                let variant_env = env.child();

                variant_env.set_assignment(AssignmentFn::new({
                    let variants = variants.clone();
                    move |left, right, env, stack| {
                        let left = Value::from(left);
                        let right = Value::from(right);

                        let name = left
                            .get_or::<Name>("Expected name", env, stack)?
                            .name
                            .clone();

                        let right = right.evaluate(env, stack)?;

                        let value_pattern = if right.is_empty() {
                            None
                        } else {
                            Some(right.get_or::<Pattern>("Expected pattern", env, stack)?)
                        };

                        variants.borrow_mut().insert(
                            name.clone(),
                            VariantConstructor {
                                name,
                                value_pattern: value_pattern.map(Cow::into_owned),
                            },
                        );

                        Ok(())
                    }
                }));

                block.reduce(&variant_env, stack)?;

                variants.take()
            } else {
                return Err(error("Expected list or block of variants", stack));
            };

            Ok(Value::of(VariantSet { id, variants }))
        })),
    )?;

    // Variant-Set == Pattern
    env.add_relation_between(stack, |variant_set: VariantSet| {
        Pattern::new(move |value, env, stack| {
            let variant = match value.get_if_present::<Variant>(env, stack)? {
                Some(value) => value.into_owned(),
                None => return Ok(None),
            };

            Ok(if variant.id == variant_set.id {
                Some(Cow::Owned(Value::of(variant)))
            } else {
                None
            })
        })
    })?;

    // Variant-Set == Module
    env.add_relation_between_with(stack, |variant_set: VariantSet, stack| {
        let env = Env::new();

        for (name, variant) in &variant_set.variants {
            env.set_variable(
                stack,
                name,
                match variant.value_pattern {
                    Some(ref pattern) => {
                        let id = variant_set.id;
                        let name = variant.name.clone();
                        let pattern = pattern.clone();

                        Value::of(Function::new(move |value, env, stack| {
                            let value = value.evaluate(env, stack)?;
                            let validated = pattern(&value, env, stack)?
                                .ok_or_else(|| error("Invalid value", stack))?;

                            Ok(Value::of(Variant {
                                id,
                                name: name.clone(),
                                value: Some(validated.into_owned()),
                            }))
                        }))
                    }
                    None => Value::of(Variant {
                        id: variant_set.id,
                        name: variant.name.clone(),
                        value: None,
                    }),
                },
            )?;
        }

        Ok(Module::new(env))
    })?;

    // Variant-Set == Text
    env.add_text_relation::<VariantSet>("variant set", stack)?;

    // Variant == Pattern
    env.add_relation_between(stack, |target: Variant| {
        Pattern::new(move |value, env, stack| {
            let variant = match value.get_if_present::<Variant>(env, stack)? {
                Some(value) => value,
                None => return Ok(None),
            }
            .into_owned();

            Ok(if variant.id == target.id && variant.name == target.name {
                Some(Cow::Owned(match variant.value {
                    Some(value) => value,
                    None => Value::of(variant),
                }))
            } else {
                None
            })
        })
    })?;

    env.add_relation_between_with(stack, {
        let env = env.clone();

        move |variant: Variant, stack| {
            Ok(match variant.value {
                Some(value) => {
                    let text = value.format(&env, stack)?;
                    Text::new(format!("{} {}", variant.name, text))
                }
                None => Text::new(variant.name),
            })
        }
    })?;

    Ok(())
}
