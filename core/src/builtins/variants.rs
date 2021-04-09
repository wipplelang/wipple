use crate::*;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, Clone)]
pub struct Variant {
    pub id: Id,
    pub name: String,
    pub associated_values: Vec<Value>,
}

impl Variant {
    pub fn new(id: Id, name: &str, associated_values: &[Value]) -> Self {
        Variant {
            id,
            name: String::from(name),
            associated_values: associated_values.to_vec(),
        }
    }
}

core_primitive!(pub variant for Variant);

impl Module {
    pub fn for_variant(items: HashMap<String, Vec<Validation>>) -> Self {
        let mut env = Environment::blank();

        for (name, validations) in items {
            fn build_constructor(
                name: String,
                id: Id,
                remaining_validations: Vec<Validation>,
                values: Vec<Value>,
            ) -> Value {
                if remaining_validations.is_empty() {
                    Value::of(Variant::new(id, &name, &values))
                } else {
                    Value::of(Function::new(move |value, env, stack| {
                        let value = value.evaluate(env, stack)?;

                        let (validation, remaining_validations) =
                            remaining_validations.split_first().unwrap();

                        let validated_value = match validation(&value, env, stack)? {
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
                build_constructor(name.clone(), Id::new(), validations, vec![]),
            );
        }

        Module::new(env)
    }
}

fn setup_match_block(matches: Rc<RefCell<HashMap<String, Value>>>, env: &EnvironmentRef) {
    *env.borrow_mut().handle_assign() = HandleAssignFn::new(move |left, right, _, _| {
        matches
            .borrow_mut()
            .insert(left.name.clone(), right.clone());

        Ok(())
    })
}

fn setup_variant_block(
    variants: Rc<RefCell<HashMap<String, Vec<Validation>>>>,
    env: &EnvironmentRef,
) {
    *env.borrow_mut().handle_assign() = HandleAssignFn::new(move |left, right, env, stack| {
        if variants.borrow().contains_key(&left.name) {
            return Err(ReturnState::Error(Error::new(
                "Cannot have two variants with the same name",
                stack,
            )));
        }

        let list = right.get_primitive_or::<List>(
            "Expected list of validations for variant",
            env,
            stack,
        )?;

        let mut validations = Vec::new();

        for item in list.items {
            let validation = item.evaluate(env, stack)?.get_primitive_or::<Validation>(
                "Expected validation in list of variant items",
                env,
                stack,
            )?;

            validations.push(validation);
        }

        variants.borrow_mut().insert(left.name.clone(), validations);

        Ok(())
    })
}

pub(crate) fn setup(env: &mut Environment) {
    env.set_variable("Variant", Value::of(Trait::of::<Variant>()));

    // TODO: Variant-Of

    env.add_conformance(Trait::variant(), Trait::text(), |value, env, stack| {
        let variant = value.clone().into_primitive::<Variant>();

        let mut associated_values = Vec::new();

        for value in variant.associated_values {
            let text = value.get_primitive_or::<Text>(
                "Variant value does not conform to Text",
                env,
                stack,
            )?;

            associated_values.push(text.text);
        }

        let text = if associated_values.is_empty() {
            variant.name
        } else {
            format!("{} {}", variant.name, associated_values.join(" "))
        };

        Ok(Value::of(Text::new(&text)))
    });

    env.set_variable(
        "variant",
        Value::of(Function::new(|value, env, stack| {
            let variants = if let Some(list) = value.get_primitive_if_present::<List>(env, stack)? {
                let mut variants = HashMap::new();

                for item in list.items {
                    let name = item.get_primitive_or::<Name>("Expected name", env, stack)?;

                    variants.insert(name.name, Vec::new());
                }

                variants
            } else if let Some(block) = value.get_primitive_if_present::<Block>(env, stack)? {
                let variant_env = Environment::child_of(env).into_ref();

                let variants = Rc::new(RefCell::new(HashMap::new()));
                setup_variant_block(variants.clone(), &variant_env);

                block.reduce_inline(&variant_env, stack)?;

                variants.take()
            } else {
                return Err(ReturnState::Error(Error::new(
                    "Expected list or module containing variants",
                    stack,
                )));
            };

            let variant = Module::for_variant(variants);

            Ok(Value::of(variant))
        })),
    );

    env.set_variable(
        "match",
        Value::of(Function::new(|value, env, stack| {
            let variant = value.evaluate(env, stack)?.get_primitive_or::<Variant>(
                "Expected variant",
                env,
                stack,
            )?;

            Ok(Value::of(Function::new(move |value, env, stack| {
                let block = value.get_primitive_or::<Block>(
                    "Expected block defining matches",
                    env,
                    stack,
                )?;

                let matches = Rc::new(RefCell::new(HashMap::new()));
                let match_env = Environment::child_of(env).into_ref();

                setup_match_block(matches.clone(), &match_env);

                block.reduce_inline(&match_env, stack)?;

                let matches = matches.take();

                let mut r#match = match matches.get(&variant.name) {
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
