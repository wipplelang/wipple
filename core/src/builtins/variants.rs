use crate::*;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(TypeInfo, Debug, Clone)]
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

impl Module {
    pub fn for_variant(items: HashMap<String, Vec<Pattern>>) -> Self {
        let mut env = env::blank();

        for (name, patterns) in items {
            fn build_constructor(
                name: String,
                id: Id,
                remaining_patterns: Vec<Pattern>,
                values: Vec<Value>,
            ) -> Value {
                if remaining_patterns.is_empty() {
                    Value::of(Variant::new(id, &name, &values))
                } else {
                    Value::of(Function::new(move |value, env, stack| {
                        let value = value.evaluate(env, stack)?;

                        let (pattern, remaining_patterns) =
                            remaining_patterns.split_first().unwrap();

                        let validated_value =
                            pattern(value, env, stack)?.into_valid().ok_or_else(|| {
                                Return::error(
                                    "Cannot use this value to represent this variant",
                                    stack,
                                )
                            })?;

                        let mut values = values.clone();
                        values.push(validated_value);

                        Ok(build_constructor(
                            name.clone(),
                            id,
                            remaining_patterns.to_vec(),
                            values,
                        ))
                    }))
                }
            }

            env.set_variable(
                &name,
                build_constructor(name.clone(), Id::new(), patterns, vec![]),
            );
        }

        Module::new(env.into())
    }
}

fn setup_match_block(matches: Rc<RefCell<HashMap<String, Value>>>, env: &Environment) {
    *env.borrow_mut().assignment() = AssignmentFn::new(move |left, right, _, _| {
        matches
            .borrow_mut()
            .insert(left.name.clone(), right.clone());

        Ok(())
    })
}

fn setup_variant_block(variants: Rc<RefCell<HashMap<String, Vec<Pattern>>>>, env: &Environment) {
    *env.borrow_mut().assignment() = AssignmentFn::new(move |left, right, env, stack| {
        if variants.borrow().contains_key(&left.name) {
            return Err(Return::error(
                "Cannot have two variants with the same name",
                stack,
            ));
        }

        let list = right.get_or::<List>("Expected list of patterns for variant", env, stack)?;

        let mut patterns = Vec::new();

        for item in list.items {
            let pattern = item.evaluate(env, stack)?.get_or::<Pattern>(
                "Expected pattern in list of variant items",
                env,
                stack,
            )?;

            patterns.push(pattern);
        }

        variants.borrow_mut().insert(left.name.clone(), patterns);

        Ok(())
    })
}

pub(crate) fn setup(env: &mut EnvironmentInner) {
    env.set_variable("Variant", Value::of(Trait::of::<Variant>()));

    // TODO: Variant-Of

    env.add_relation(
        Pattern::for_trait(Trait::of::<Variant>()),
        Trait::of::<Text>(),
        |value, env, stack| {
            let variant = value.into_primitive::<Variant>().unwrap();

            let mut associated_values = Vec::new();

            for value in variant.associated_values {
                let text = value.get_or::<Text>(
                    "Variant value cannot be represented as Text",
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
        },
    );

    env.set_variable(
        "variant",
        Value::of(Function::new(|value, env, stack| {
            let variants = if let Some(list) = value.get_if_present::<List>(env, stack)? {
                let mut variants = HashMap::new();

                for item in list.items {
                    let name = item.get_or::<Name>("Expected name", env, stack)?;

                    variants.insert(name.name, Vec::new());
                }

                variants
            } else if let Some(block) = value.get_if_present::<Block>(env, stack)? {
                let variant_env = env::child_of(env).into();

                let variants = Rc::new(RefCell::new(HashMap::new()));
                setup_variant_block(variants.clone(), &variant_env);

                block.reduce(&variant_env, stack)?;

                variants.take()
            } else {
                return Err(Return::error(
                    "Expected list or module containing variants",
                    stack,
                ));
            };

            let variant = Module::for_variant(variants);

            Ok(Value::of(variant))
        })),
    );

    env.set_variable(
        "match",
        Value::of(Function::new(|value, env, stack| {
            let variant =
                value
                    .evaluate(env, stack)?
                    .get_or::<Variant>("Expected variant", env, stack)?;

            Ok(Value::of(Function::new(move |value, env, stack| {
                let block = value.get_or::<Block>("Expected block defining matches", env, stack)?;

                let matches = Rc::new(RefCell::new(HashMap::new()));
                let match_env = env::child_of(env).into();

                setup_match_block(matches.clone(), &match_env);

                block.reduce(&match_env, stack)?;

                let matches = matches.take();

                let mut r#match = match matches.get(&variant.name) {
                    Some(value) => value.evaluate(env, stack),
                    None => Err(Return::error(
                        &format!("No match for variant '{}'", variant.name),
                        stack,
                    )),
                }?;

                for value in variant.associated_values.clone() {
                    r#match = r#match.call(value, env, stack)?;
                }

                Ok(r#match)
            })))
        })),
    );
}
