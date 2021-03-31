use wipple::*;

pub fn prelude(env: &EnvironmentRef) {
    // Trait functions

    env.borrow_mut().set_variable(
        "trait",
        Value::of(Function::new(|value, env, stack| {
            let validation = value
                .evaluate(env, stack.clone())?
                .get_primitive_or::<Validation>("Expected validation", env, stack)?;

            Ok(Value::of(TraitConstructor {
                r#trait: Trait::new(),
                validation,
            }))
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

    let for_operator = Operator::collect(|trait_constructor, value, env, stack| {
        let trait_constructor = trait_constructor
            .evaluate(env, stack.clone())?
            .get_primitive_or::<TraitConstructor>("Expected trait", env, stack.clone())?;

        let trait_value = value.evaluate(env, stack.clone())?.get_trait_or(
            &trait_constructor.r#trait,
            "Value does not have trait",
            env,
            stack,
        )?;

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
        ($operation:tt, $precedence_group:ident) => {
            boolean_math!(stringify!($operation), $operation, $precedence_group)
        };
        ($name:expr, $operation:tt, $precedence_group:ident) => {{
            let operator = Operator::collect(|left, right, env, stack| {
                let left = left
                    .evaluate(env, stack.clone())?
                    .get_primitive::<Number>(env, stack.clone())?;

                let right = right
                    .evaluate(env, stack.clone())?
                    .get_primitive::<Number>(env, stack.clone())?;

                let result = left.number $operation right.number;

                Ok(Value::from_bool(result))
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

    boolean_math!(>, comparison_precedence_group);
    boolean_math!(<, comparison_precedence_group);
    boolean_math!("=", ==, comparison_precedence_group);
}
