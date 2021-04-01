mod boolean;
mod maybe;
mod result;

use crate::*;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

struct VariantKind;

impl Kind {
    pub fn variant() -> Self {
        Kind::of_with_parent::<VariantKind>(&Kind::Any)
    }
}

#[derive(Clone)]
pub struct Variant {
    pub variant_id: Id,
    pub name: String,
    pub associated_values: Vec<Value>,
}

impl Variant {
    pub fn new(variant_id: Id, name: &str, associated_values: &[Value]) -> Self {
        Variant {
            variant_id,
            name: String::from(name),
            associated_values: associated_values.to_vec(),
        }
    }
}

impl Primitive for Variant {}

impl Module {
    pub fn for_variant_of(id: Id, items: HashMap<String, Vec<Validation>>) -> Self {
        let mut env = Environment::blank();

        for (name, validations) in items {
            fn build_constructor(
                name: String,
                id: Id,
                remaining_validations: Vec<Validation>,
                values: Vec<Value>,
            ) -> Value {
                if remaining_validations.is_empty() {
                    let variant_trait = Trait {
                        id,
                        kind: Kind::variant(),
                    };

                    Value::new(
                        variant_trait,
                        Dynamic::new(Variant::new(id, &name, &values)),
                    )
                } else {
                    Value::of(Function::new(move |value, env, stack| {
                        let (validation, remaining_validations) =
                            remaining_validations.split_first().unwrap();

                        let validated_value = match validation(
                            &value.evaluate(env, stack.clone())?,
                            env,
                            stack.clone(),
                        )? {
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
        Module::for_variant_of(Id::new(), items)
    }
}

fn setup_match_block(matches: Rc<RefCell<HashMap<String, Value>>>, env: &EnvironmentRef) {
    *env.borrow_mut().handle_assign() = HandleAssignFn::new(
        move |left, right, computed, env, stack| {
            let name =
                left.get_primitive_or::<Name>("Expected name for match", env, stack.clone())?;

            if computed {
                return Err(ReturnState::Error(Error::new(
                    "Matches are already lazily evaluated, so creating a computed variable here is unnecessary",
                    stack
                )));
            }

            matches.borrow_mut().insert(name.name, right.clone());

            Ok(())
        },
    )
}

fn setup_variant_block(
    variants: Rc<RefCell<HashMap<String, Vec<Validation>>>>,
    env: &EnvironmentRef,
) {
    *env.borrow_mut().handle_assign() =
        HandleAssignFn::new(move |left, right, computed, env, stack| {
            let name =
                left.get_primitive_or::<Name>("Expected name for variant", env, stack.clone())?;

            if computed {
                return Err(ReturnState::Error(Error::new(
                    "Lazy evaluation is not supported for variants",
                    stack,
                )));
            }

            if variants.borrow().contains_key(&name.name) {
                return Err(ReturnState::Error(Error::new(
                    "Cannot have two variants with the same name",
                    stack,
                )));
            }

            let list = right.get_primitive_or::<List>(
                "Expected list of validations for variant",
                env,
                stack.clone(),
            )?;

            let mut validations = Vec::new();

            for item in list.items {
                let validation = item
                    .evaluate(env, stack.clone())?
                    .get_primitive_or::<Validation>(
                        "Expected validation in list of variant items",
                        env,
                        stack.clone(),
                    )?;

                validations.push(validation);
            }

            variants.borrow_mut().insert(name.name, validations);

            Ok(())
        })
}

pub(crate) fn setup(env: &mut Environment) {
    boolean::setup(env);
    maybe::setup(env);
    result::setup(env);

    env.conformances().push(Conformance::new(
        ConformanceMatch::Kind(Kind::variant()),
        Trait::text(),
        |value, env, stack| {
            let variant = value.clone().into_primitive::<Variant>();

            let mut associated_values = Vec::new();

            for value in variant.associated_values {
                let text = value.get_primitive_or::<Text>(
                    "Variant value does not conform to Text",
                    env,
                    stack.clone(),
                )?;

                associated_values.push(text.text);
            }

            let text = if associated_values.is_empty() {
                variant.name
            } else {
                format!("{} {}", variant.name, associated_values.join(" "))
            };

            Ok(Value::of(Text::new(&text)))
        },
    ));

    env.set_variable(
        "variant",
        Value::of(Function::new(|value, env, stack| {
            let variants =
                if let Some(list) = value.get_primitive_if_present::<List>(env, stack.clone())? {
                    let mut variants = HashMap::new();

                    for item in list.items {
                        let name =
                            item.get_primitive_or::<Name>("Expected name", env, stack.clone())?;

                        variants.insert(name.name, Vec::new());
                    }

                    variants
                } else if let Some(block) =
                    value.get_primitive_if_present::<Block>(env, stack.clone())?
                {
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

            let variant = Module::for_variant_of(Id::new(), variants);

            Ok(Value::of(variant))
        })),
    );

    env.set_variable(
        "match",
        Value::of(Function::new(|value, env, stack| {
            let variant = value
                .evaluate(env, stack.clone())?
                .get_kind_or(&Kind::variant(), "Expected variant", env, stack)?
                .into_primitive::<Variant>();

            Ok(Value::of(Function::new(move |value, env, stack| {
                let block = value.get_primitive_or::<Block>(
                    "Expected block defining matches",
                    env,
                    stack.clone(),
                )?;

                let matches = Rc::new(RefCell::new(HashMap::new()));
                let match_env = Environment::child_of(env).into_ref();

                setup_match_block(matches.clone(), &match_env);

                block.reduce_inline(&match_env, stack.clone())?;

                let matches = matches.take();

                let mut r#match = match matches.get(&variant.name) {
                    Some(value) => value.evaluate(env, stack.clone()),
                    None => Err(ReturnState::Error(Error::new(
                        &format!("No match for variant '{}'", variant.name),
                        stack.clone(),
                    ))),
                }?;

                for value in &variant.associated_values {
                    r#match = r#match.call(value, env, stack.clone())?;
                }

                Ok(r#match)
            })))
        })),
    );
}
