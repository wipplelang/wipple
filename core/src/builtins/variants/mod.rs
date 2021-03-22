mod boolean;
mod maybe;
mod result;

use crate::*;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Clone)]
pub struct Variant {
    pub variant_id: TraitID,
    pub name: String,
    pub associated_values: Vec<Value>,
}

impl Variant {
    pub fn new(variant_id: TraitID, name: &str, associated_values: &[Value]) -> Self {
        Variant {
            variant_id,
            name: String::from(name),
            associated_values: associated_values.to_vec(),
        }
    }
}

impl Primitive for Variant {}

impl Module {
    pub fn for_variant_of(id: TraitID, items: HashMap<String, Vec<Validation>>) -> Self {
        let mut env = Environment::blank();

        for (name, validations) in items {
            fn build_constructor(
                name: String,
                id: TraitID,
                remaining_validations: Vec<Validation>,
                values: Vec<Value>,
            ) -> Value {
                if remaining_validations.is_empty() {
                    let mut variant_trait = Trait::new(id, {
                        let name = name.clone();
                        move |_, _| Ok(Value::of(Variant::new(id, &name, &values)))
                    });

                    variant_trait.is_variant = true;

                    let text_trait =
                        Trait::of_primitive(Text::new(&format!("<variant '{}'>", name)));

                    Value::new(&variant_trait).add(&text_trait)
                } else {
                    Value::of(Function::new(move |value, env, stack| {
                        let (validation, remaining_validations) =
                            remaining_validations.split_first().unwrap();

                        let validated_value = match validation(value, env, stack)? {
                            Validated::Valid(value) => value,
                            Validated::Invalid => {
                                return Err(ReturnState::Error(Error::new(
                                    "Cannot use this value to represent this variant",
                                    stack,
                                )))
                            }
                        };

                        let mut values = values.clone();
                        values.push(validated_value);

                        Ok(build_constructor(
                            name.clone(),
                            id,
                            remaining_validations.to_vec(),
                            values,
                        ))
                    }))
                }
            }

            env.set_variable(
                &name,
                build_constructor(name.clone(), id, validations, vec![]),
            );
        }

        Module::new(env)
    }

    pub fn for_variant(items: HashMap<String, Vec<Validation>>) -> Self {
        Module::for_variant_of(TraitID::new(), items)
    }
}

fn setup_match_block(env: &mut Environment, matches: Rc<RefCell<HashMap<String, Value>>>) {
    *env.handle_assign() = HandleAssignFn::new(move |left, right, computed, env, stack| {
        let name = left.get_primitive_or::<Name>("Expected name for match", env, &stack)?;

        if computed {
            return Err(ReturnState::Error(Error::new(
                "Matches are already lazily evaluated, so creating a computed variable here is unnecessary",
                stack
            )));
        }

        matches.borrow_mut().insert(name.name, right.clone());

        Ok(())
    })
}

pub(crate) fn setup(env: &mut Environment) {
    boolean::setup(env);
    maybe::setup(env);
    result::setup(env);

    env.set_variable(
        "match",
        Value::of(Function::new(|value, env, stack| {
            // FIXME: Only works with traits directly defined on the value
            let variant_traits = value
                .evaluate(env, stack)?
                .traits()
                .into_iter()
                .filter(|t| t.is_variant)
                .collect::<Vec<_>>();

            let variant =
                (match variant_traits.len() {
                    1 => variant_traits[0].clone(),
                    0 => return Err(ReturnState::Error(Error::new("Expected variant", stack))),
                    _ => return Err(ReturnState::Error(Error::new(
                        "Value contains multiple variants, so the variant to match is ambiguous",
                        stack,
                    ))),
                }
                .value)(env, stack)?
                .cast_primitive::<Variant>();

            Ok(Value::of(Function::new(move |value, env, stack| {
                let block = value.get_primitive_or::<Block>(
                    "Expected block defining matches",
                    env,
                    stack,
                )?;

                let matches = Rc::new(RefCell::new(HashMap::new()));
                let mut match_env = Environment::child_of(env);
                setup_match_block(&mut match_env, matches.clone());

                block.reduce_inline(&match_env.into_ref(), stack)?;

                let mut r#match = match matches.borrow().get(&variant.name) {
                    Some(value) => value.evaluate(env, stack),
                    None => Err(ReturnState::Error(Error::new(
                        &format!("No match for variant '{}'", variant.name),
                        stack,
                    ))),
                }?;

                for value in &variant.associated_values {
                    r#match = r#match.call(value, env, stack)?;
                }

                Ok(r#match)
            })))
        })),
    );
}
