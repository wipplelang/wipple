use crate::*;

pub fn prelude() -> Environment {
    let mut env = Environment::blank();

    setup(&mut env);
    temporary_prelude(&mut env);

    // TODO: Load bundled prelude files

    env
}

// FIXME: Temporary
fn temporary_prelude(env: &mut Environment) {
    // 'new' function
    fn add(
        base: &Value,
        trait_constructor: &TraitConstructor,
        value: &Value,
        env: &EnvironmentRef,
        stack: &Stack,
    ) -> Result {
        let validated_value = match trait_constructor.validation.0(value, env, stack)? {
            Validated::Valid(value) => value,
            Validated::Invalid => {
                return Err(Error::new(
                    "Cannot use this value to represent this trait",
                    stack,
                ))
            }
        };

        let r#trait = Trait {
            id: trait_constructor.id,
            value: validated_value,
        };

        Ok(base.add(&r#trait))
    }

    env.set_variable(
        "new",
        Value::of(Function::new(|value, env, stack| {
            let trait_constructor = value
                .evaluate(env, stack)?
                .get_primitive::<TraitConstructor>(env, stack)?;

            Ok(Value::of(Function::new(move |value, env, stack| {
                add(
                    &Value::empty(),
                    &trait_constructor,
                    &value.evaluate(env, stack)?,
                    env,
                    stack,
                )
            })))
        })),
    );

    // 'do' function

    env.set_variable(
        "do",
        Value::of(Function::new(|value, env, stack| {
            let inner_env = Environment::child_of(env).into_ref();
            value.evaluate(&inner_env, stack)
        })),
    );

    // 'use' function

    env.set_variable(
        "use",
        Value::of(Function::new(|value, env, stack| {
            let module = value
                .evaluate(env, stack)?
                .get_primitive::<Module>(env, stack)?;

            env.borrow_mut().r#use(&module.values);

            Ok(Value::empty())
        })),
    );

    // Assignment operator (::)

    fn group(list: &[Value]) -> Value {
        if list.len() == 1 {
            list[0].clone()
        } else {
            Value::of(List {
                items: list.to_vec(),
                location: None,
            })
        }
    }

    fn assign(left: &Value, right: &Value, env: &EnvironmentRef, stack: &Stack) -> Result {
        let stack = stack.add(|| {
            format!(
                "Assigning '{}' to '{}",
                right.format(env, stack),
                left.format(env, stack)
            )
        });

        let assign = left.get_primitive_or::<AssignFn>(
            "Cannot assign to this value because it does not have the Assign trait",
            env,
            &stack,
        )?;

        assign.0(right, env, &stack)?;

        Ok(Value::empty())
    }

    let assignment_precedence_group = env.add_precedence_group(
        Associativity::Right,
        PrecedenceGroupComparison::<VariadicPrecedenceGroup>::highest(),
    );

    let assignment_operator = VariadicOperator::collect(|left, right, env, stack| {
        assign(
            &group(left),
            &group(right).evaluate(env, stack)?,
            env,
            stack,
        )
    });

    env.add_variadic_operator(&assignment_operator, &assignment_precedence_group);
    env.set_variable(":", Value::of(Operator::Variadic(assignment_operator)));

    // Add trait operator (::)

    let add_trait_operator = VariadicOperator::collect(|left, right, env, stack| {
        let value = group(left);

        // TODO: Auto-derive a value for the trait using the conformance if only
        // the trait is provided
        if right.len() != 2 {
            return Err(Error::new(
                "Expected a trait and a value for the trati",
                stack,
            ));
        }

        let trait_constructor_value = right[0].evaluate(env, stack)?;
        let trait_value = right[1].evaluate(env, stack)?;

        let stack = stack.add(|| {
            format!(
                "Adding trait '{}' with '{}' to '{}'",
                trait_constructor_value.format(env, stack),
                trait_value.format(env, stack),
                value.format(env, stack)
            )
        });

        let trait_constructor =
            trait_constructor_value.get_primitive::<TraitConstructor>(env, &stack)?;

        let new_value = add(&value, &trait_constructor, &trait_value, env, &stack)?;

        assign(
            &value,
            // We have to quote the result because we've already evaluated it;
            // in real Wipple code, the result would be assigned to a variable
            // before being passed here and we wouldn't have this problem
            &Value::of(Quoted {
                value: new_value,
                location: None,
            }),
            env,
            &stack,
        )
    });

    env.add_variadic_operator(&add_trait_operator, &assignment_precedence_group);
    env.set_variable("::", Value::of(Operator::Variadic(add_trait_operator)));

    // Macro operator (=>)

    let function_precedence_group = env.add_precedence_group(
        Associativity::Right,
        PrecedenceGroupComparison::<VariadicPrecedenceGroup>::lower_than(
            assignment_precedence_group,
        ),
    );

    let macro_operator = VariadicOperator::collect(|left, right, env, stack| {
        let define_parameter = group(left).get_primitive_or::<DefineMacroParameterFn>(
            "Macro parameter must have the Macro-Parameter trait",
            env,
            stack,
        )?;

        Ok(Value::of(Macro {
            define_parameter,
            value_to_expand: group(right),
        }))
    });

    env.add_variadic_operator(&macro_operator, &function_precedence_group);
    env.set_variable("=>", Value::of(Operator::Variadic(macro_operator)));

    // Closure operator (->)

    let closure_operator = VariadicOperator::collect(|left, right, env, stack| {
        let define_parameter = group(left).get_primitive_or::<AssignFn>(
            "Closure parameter must have the Assign trait",
            env,
            stack,
        )?;

        let return_value = group(right);

        let outer_env = env.to_owned();

        Ok(Value::of(Function::new(move |value, _, stack| {
            let inner_env = Environment::child_of(&outer_env).into_ref();

            define_parameter.0(value, &inner_env, stack)?;
            return_value.evaluate(&inner_env, stack)
        })))
    });

    env.add_variadic_operator(&closure_operator, &function_precedence_group);
    env.set_variable("->", Value::of(Operator::Variadic(closure_operator)));

    // Math

    macro_rules! math {
        ($operation:tt, $precedence_group:ident) => {{
            let operator = BinaryOperator::collect(|left, right, env, stack| {
                let left = left
                    .evaluate(env, stack)?
                    .get_primitive::<Number>(env, stack)?;

                let right = right
                    .evaluate(env, stack)?
                    .get_primitive::<Number>(env, stack)?;

                let result = left.number $operation right.number;

                Ok(Value::of(Number {
                    number: result,
                    location: None,
                }))
            });

            env.add_binary_operator(&operator, &$precedence_group);

            env.set_variable(
                stringify!($operation),
                Value::of(Operator::Binary(operator)),
            );
        }};
    }

    let addition_precedence_group = env.add_precedence_group(
        Associativity::Left,
        PrecedenceGroupComparison::<BinaryPrecedenceGroup>::lowest(),
    );

    math!(+, addition_precedence_group);
    math!(-, addition_precedence_group);

    let multiplication_precedence_group = env.add_precedence_group(
        Associativity::Left,
        PrecedenceGroupComparison::<BinaryPrecedenceGroup>::higher_than(addition_precedence_group),
    );

    math!(*, multiplication_precedence_group);
    math!(/, multiplication_precedence_group);
}
