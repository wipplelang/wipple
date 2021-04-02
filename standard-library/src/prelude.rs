use wipple::*;

pub fn prelude(env: &EnvironmentRef) {
    // Trait functions

    env.borrow_mut().set_variable(
        "trait",
        Value::of(Function::new(|value, env, stack| {
            let validation = value
                .evaluate(env, stack.clone())?
                .get_primitive_or::<Validation>("Expected validation", env, stack)?;

            Ok(Value::of(Trait::new(validation)))
        })),
    );

    // Block functions

    env.borrow_mut().set_variable(
        "do",
        Value::of(Function::new(|value, env, stack| {
            let block = value.get_primitive_or::<Block>("Expected block", env, stack.clone())?;
            block.reduce(env, stack)
        })),
    );

    env.borrow_mut().set_variable(
        "inline",
        Value::of(Function::new(|value, env, stack| {
            let block = value.get_primitive_or::<Block>("Expected block", env, stack.clone())?;
            block.reduce_inline(env, stack)
        })),
    );

    // 'use' function

    env.borrow_mut().set_variable(
        "use",
        Value::of(Function::new(|value, env, stack| {
            let module = value
                .evaluate(env, stack.clone())?
                .get_primitive_or::<Module>("Expected a module", env, stack)?;

            env.borrow_mut().r#use(&module.env.borrow());

            Ok(Value::empty())
        })),
    );

    // Assignment operators

    let assignment_precedence_group =
        add_precedence_group(Associativity::Right, PrecedenceGroupComparison::highest());

    let add_assignment_operator = |name: &str, computed: bool, env: &EnvironmentRef| {
        let operator = Operator::collect(move |left, right, env, stack| {
            let handle_assign = env.borrow_mut().handle_assign().clone();
            handle_assign(left, right, computed, env, stack)?;
            Ok(Value::empty())
        });

        add_operator(&operator, &assignment_precedence_group);

        env.borrow_mut().set_variable(name, Value::of(operator));
    };

    add_assignment_operator(":", false, env);
    add_assignment_operator(":>", true, env);

    // Conformance operator (==)

    let conformance_operator = Operator::collect(|left, right, env, stack| {
        let (matching_trait, name) =
            match left.get_primitive_if_present::<List>(env, stack.clone())? {
                Some(left) if left.items.len() == 2 => {
                    let matching_trait = left.items[0]
                        .evaluate(env, stack.clone())?
                        .get_primitive_or::<Trait>("Expected trait", env, stack.clone())?;

                    let name = left.items[1]
                        .get_primitive_or::<Name>("Expected name", env, stack.clone())?
                        .name;

                    (matching_trait, name)
                }
                _ => {
                    return Err(ReturnState::Error(Error::new(
                        "Expected conformance predicate in the form 'T x'",
                        stack,
                    )))
                }
            };

        let right = right.get_primitive_or::<List>(
            "Expected a list containing the value to derive",
            env,
            stack.clone(),
        )?;

        if right.items.len() != 2 {
            return Err(ReturnState::Error(Error::new(
                "Expected a value to derive and its trait",
                stack,
            )));
        }

        let derived_trait = right.items[0]
            .evaluate(env, stack.clone())?
            .get_primitive_or::<Trait>("Expected trait", env, stack)?;

        let derived_value = right.items[1].clone();

        let derive_env = Environment::child_of(env).into_ref();
        let check_env = env.clone();

        env.borrow_mut().add_conformance(
            matching_trait,
            derived_trait.clone(),
            move |value, _, stack| {
                derive_env.borrow_mut().set_variable(&name, value.clone());

                let derived_value = derived_value.evaluate(&derive_env, stack.clone())?;

                Value::new_validated(derived_trait.clone(), derived_value, &check_env, stack)
            },
        );

        Ok(Value::empty())
    });

    add_operator(&conformance_operator, &assignment_precedence_group);

    env.borrow_mut()
        .set_variable("==", Value::of(conformance_operator));

    // Template operator (=>)

    let function_precedence_group = add_precedence_group(
        Associativity::Right,
        PrecedenceGroupComparison::lower_than(assignment_precedence_group),
    );

    let template_operator = Operator::collect(|parameter, value_to_expand, env, stack| {
        let name =
            parameter.get_primitive_or::<Name>("Template parameter must be a name", env, stack)?;

        Ok(Value::of(Template {
            parameter: name.name,
            replace_in: value_to_expand.clone(),
        }))
    });

    add_operator(&template_operator, &function_precedence_group);

    env.borrow_mut()
        .set_variable("=>", Value::of(template_operator));

    // Closure operator (->)

    let closure_operator = Operator::collect(|parameter, return_value, env, stack| {
        let define_parameter = parameter.get_primitive_or::<AssignFn>(
            "Closure parameter must have the Assign trait",
            env,
            stack,
        )?;

        let captured_env = env.clone();

        Ok(Value::of(Closure {
            captured_env,
            define_parameter,
            return_value: return_value.clone(),
        }))
    });

    add_operator(&closure_operator, &function_precedence_group);

    env.borrow_mut()
        .set_variable("->", Value::of(closure_operator));

    // 'for' operator

    let for_precedence_group = add_precedence_group(
        Associativity::Right,
        PrecedenceGroupComparison::lower_than(function_precedence_group),
    );

    let for_operator = Operator::collect(|r#trait, value, env, stack| {
        let r#trait = r#trait
            .evaluate(env, stack.clone())?
            .get_primitive_or::<Trait>("Expected trait", env, stack.clone())?;

        let trait_value = value
            .evaluate(env, stack.clone())?
            .get_trait_or(&r#trait, "Value does not have trait", env, stack)?
            .contained_value()
            .clone();

        Ok(trait_value)
    });

    add_operator(&for_operator, &for_precedence_group);

    env.borrow_mut()
        .set_variable("for", Value::of(for_operator));

    // Math

    macro_rules! math {
        ($operation:tt, $precedence_group:ident) => {{
            let operator = Operator::collect(|left, right, env, stack| {
                let left = left
                    .evaluate(env, stack.clone())?
                    .get_primitive::<Number>(env, stack.clone())?;

                let right = right
                    .evaluate(env, stack.clone())?
                    .get_primitive::<Number>(env, stack.clone())?;

                let result = left.number $operation right.number;

                Ok(Value::of(Number::new(result)))
            });

            add_operator(&operator, &$precedence_group);

            env.borrow_mut().set_variable(
                stringify!($operation),
                Value::of(operator),
            );
        }};
    }

    let addition_precedence_group =
        add_precedence_group(Associativity::Left, PrecedenceGroupComparison::lowest());

    math!(+, addition_precedence_group);
    math!(-, addition_precedence_group);

    let multiplication_precedence_group = add_precedence_group(
        Associativity::Left,
        PrecedenceGroupComparison::higher_than(addition_precedence_group),
    );

    math!(*, multiplication_precedence_group);
    math!(/, multiplication_precedence_group);

    macro_rules! boolean_math {
        ($operation:tt, $precedence_group:ident, $prelude_env:expr) => {
            boolean_math!(stringify!($operation), $operation, $precedence_group, $prelude_env)
        };
        ($name:expr, $operation:tt, $precedence_group:ident, $prelude_env:expr) => {{
            let operator = Operator::collect(|left, right, env, stack| {
                let left = left
                    .evaluate(env, stack.clone())?
                    .get_primitive::<Number>(env, stack.clone())?;

                let right = right
                    .evaluate(env, stack.clone())?
                    .get_primitive::<Number>(env, stack.clone())?;

                let result = left.number $operation right.number;

                // This will always work because 'true' and 'false' are defined
                // inside the standard library, which we have full control over
                let value = Name::new(&result.to_string())
                    .resolve(env, stack)
                    .expect("'true' and 'false' somehow aren't defined");

                Ok(value)
            });

            add_operator(&operator, &$precedence_group);

            env.borrow_mut().set_variable(
                $name,
                Value::of(operator),
            );
        }};
    }

    let comparison_precedence_group = add_precedence_group(
        Associativity::Right,
        PrecedenceGroupComparison::higher_than(multiplication_precedence_group),
    );

    boolean_math!(>, comparison_precedence_group, env);
    boolean_math!(<, comparison_precedence_group, env);
    boolean_math!("=", ==, comparison_precedence_group, env);

    // 'format' function

    env.borrow_mut().set_variable(
        "format",
        Value::of(Function::new(|value, env, stack| {
            let format_text = value.get_primitive_or::<Text>("Expected format text", env, stack)?;

            fn build_formatter(remaining_strings: Vec<String>, result: String) -> Value {
                if remaining_strings.len() == 1 {
                    Value::of(Text::new(&(result + &remaining_strings[0])))
                } else {
                    Value::of(Function::new(move |value, env, stack| {
                        let (leading_string, remaining_strings) =
                            remaining_strings.split_first().unwrap();

                        let text = value
                            .evaluate(env, stack.clone())?
                            .get_primitive_or::<Text>(
                                "Cannot format this value because it does not conform to Text",
                                env,
                                stack,
                            )?;

                        Ok(build_formatter(
                            remaining_strings.to_vec(),
                            result.clone() + leading_string + &text.text,
                        ))
                    }))
                }
            }

            let strings = format_text.text.split('_').map(String::from).collect();

            Ok(build_formatter(strings, String::from("")))
        })),
    )
}
