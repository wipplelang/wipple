use crate::*;

pub fn setup() {
    let env = Environment::global();
    *env.borrow_mut() = Environment::blank();

    builtins::setup(&mut env.borrow_mut());
    temporary_prelude(&env);

    // TODO: Load bundled prelude files
}

// FIXME: Temporary
fn temporary_prelude(env: &EnvironmentRef) {
    // 'new' function
    fn add(
        base: &Value,
        trait_constructor: TraitConstructor,
        value: Value,
        env: &EnvironmentRef,
    ) -> Value {
        let captured_env = Environment::child_of(env).into_ref();

        let r#trait = Trait::new(
            trait_constructor.id,
            move |_, stack| match trait_constructor.validation.0(&value, &captured_env, stack)? {
                Validated::Valid(value) => Ok(value),
                Validated::Invalid => Err(ReturnState::Error(Error::new(
                    "Cannot use this value to represent this trait",
                    stack,
                ))),
            },
        );

        base.add(&r#trait)
    }

    env.borrow_mut().set_variable(
        "new",
        Value::of(Function::new(|value, env, stack| {
            let trait_constructor = value
                .evaluate(env, stack)?
                .get_primitive::<TraitConstructor>(env, stack)?;

            Ok(Value::of(Function::new(move |value, env, stack| {
                Ok(add(
                    &Value::empty(),
                    trait_constructor.clone(),
                    value.evaluate(env, stack)?,
                    env,
                ))
            })))
        })),
    );

    // 'do' function

    env.borrow_mut().set_variable(
        "do",
        Value::of(Function::new(|value, env, stack| {
            let inner_env = Environment::child_of(env).into_ref();
            value.evaluate(&inner_env, stack)
        })),
    );

    // 'use' function

    env.borrow_mut().set_variable(
        "use",
        Value::of(Function::new(|value, env, stack| {
            let module = value.evaluate(env, stack)?.get_primitive_or::<Module>(
                "Expected a module",
                env,
                stack,
            )?;

            env.borrow_mut().r#use(&module.env.borrow());

            Ok(Value::empty())
        })),
    );

    // Assignment operator (::)

    fn assign(left: &Value, right: &Value, env: &EnvironmentRef, stack: &Stack) -> Result {
        let stack = stack.add(|| {
            format!(
                "Assigning '{}' to '{}'",
                right.try_format(env, stack),
                left.try_format(env, stack)
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

    let assignment_precedence_group =
        add_precedence_group(Associativity::Right, PrecedenceGroupComparison::highest());

    let assignment_operator = Operator::collect(assign);

    add_operator(&assignment_operator, &assignment_precedence_group);

    env.borrow_mut()
        .set_variable(":", Value::of(assignment_operator));

    // Add trait operator (::)

    let add_trait_operator = Operator::collect(|value, right, env, stack| {
        let (trait_constructor_value, trait_value) =
            match right.get_primitive_if_present::<List>(env, stack)? {
                Some(right) if right.items.len() == 2 => (
                    right.items[0].evaluate(env, stack)?,
                    right.items[1].evaluate(env, stack)?,
                ),
                _ => {
                    return Err(ReturnState::Error(Error::new(
                        "Expected a trait and a value for the trati",
                        stack,
                    )))
                }
            };

        let stack = stack.add(|| {
            format!(
                "Adding trait '{}' with '{}' to '{}'",
                trait_constructor_value.try_format(env, stack),
                trait_value.try_format(env, stack),
                value.try_format(env, stack)
            )
        });

        let trait_constructor =
            trait_constructor_value.get_primitive::<TraitConstructor>(env, &stack)?;

        let new_value = add(&value, trait_constructor, trait_value, env);

        assign(
            &value,
            // We have to quote the result because we've already evaluated it;
            // in real Wipple code, the result would be assigned to a variable
            // before being passed here and we wouldn't have this problem
            &Value::of(Quoted::new(new_value)),
            env,
            &stack,
        )
    });

    add_operator(&add_trait_operator, &assignment_precedence_group);

    env.borrow_mut()
        .set_variable("::", Value::of(add_trait_operator));

    // Macro operator (=>)

    let function_precedence_group = add_precedence_group(
        Associativity::Right,
        PrecedenceGroupComparison::lower_than(assignment_precedence_group),
    );

    let macro_operator = Operator::collect(|parameter, value_to_expand, env, stack| {
        let define_parameter = parameter.get_primitive_or::<DefineMacroParameterFn>(
            "Macro parameter must have the Macro-Parameter trait",
            env,
            stack,
        )?;

        Ok(Value::of(Macro {
            define_parameter,
            value_to_expand: value_to_expand.clone(),
        }))
    });

    add_operator(&macro_operator, &function_precedence_group);

    env.borrow_mut()
        .set_variable("=>", Value::of(macro_operator));

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

    // Math

    macro_rules! math {
        ($operation:tt, $precedence_group:ident) => {{
            let operator = Operator::collect(|left, right, env, stack| {
                let left = left
                    .evaluate(env, stack)?
                    .get_primitive::<Number>(env, stack)?;

                let right = right
                    .evaluate(env, stack)?
                    .get_primitive::<Number>(env, stack)?;

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
}
