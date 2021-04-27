use wipple::*;

pub fn prelude(env: &EnvironmentRef) {
    // Trait functions

    env.borrow_mut().set_variable(
        "trait",
        Value::of(Function::new(|value, env, stack| {
            let validation = value.evaluate(env, stack)?.get_or::<Validation>(
                "Expected validation",
                env,
                stack,
            )?;

            Ok(Value::of(Trait::new(validation)))
        })),
    );

    // Validation functions

    env.borrow_mut().set_variable(
        "Is",
        Value::of(Function::new(|value, env, stack| {
            let validation = value.evaluate(env, stack)?.get_or::<Validation>(
                "Expected validation",
                env,
                stack,
            )?;

            Ok(Value::of(Validation::is(validation)))
        })),
    );

    // Block functions

    env.borrow_mut().set_variable(
        "do",
        Value::of(Function::new(|value, env, stack| {
            let block = value.get_or::<Block>("Expected block", env, stack)?;
            block.reduce(env, stack)
        })),
    );

    env.borrow_mut().set_variable(
        "inline",
        Value::of(Function::new(|value, env, stack| {
            let block = value.get_or::<Block>("Expected block", env, stack)?;
            block.reduce_inline(env, stack)
        })),
    );

    // 'use' function

    env.borrow_mut().set_variable(
        "use",
        Value::of(Function::new(|value, env, stack| {
            let module =
                value
                    .evaluate(env, stack)?
                    .get_or::<Module>("Expected a module", env, stack)?;

            env.borrow_mut().r#use(&module.env.borrow());

            Ok(Value::empty())
        })),
    );

    // Assignment operators

    let assignment_precedence_group = add_variadic_precedence_group(
        Associativity::Right,
        VariadicPrecedenceGroupComparison::highest(),
    );

    macro_rules! assignment_operator {
        ($name:expr, $handle:ident, $env:expr) => {
            let operator = VariadicOperator::collect(move |left, right, env, stack| {
                let left = match left {
                    VariadicOperatorInput::Single(left) => left,
                    _ => {
                        return Err(Return::error(
                            "Expected single value on left side of assignment",
                            stack,
                        ))
                    }
                };

                let name = left.get_or::<Name>("Expected name", env, stack)?;

                let handle = env.borrow_mut().$handle().clone();
                handle(&name, &right.into_value(), env, stack)?;

                Ok(Value::empty())
            });

            add_variadic_operator(&operator, &assignment_precedence_group);

            $env.borrow_mut()
                .set_variable($name, Value::of(Operator::Variadic(operator)));
        };
    }

    assignment_operator!(":", handle_assign, env);
    assignment_operator!(":>", handle_computed_assign, env);

    // Conformance operator (==)

    let conformance_operator = VariadicOperator::collect(|left, right, env, stack| {
        let (matching_trait, name) = match left {
            VariadicOperatorInput::Single(left) => {
                let matching_trait =
                    left.evaluate(env, stack)?
                        .get_or::<Trait>("Expected trait", env, stack)?;

                (matching_trait, None)
            }
            VariadicOperatorInput::List(left) => {
                if left.len() != 2 {
                    return Err(Return::error(
                        "Expected conformance predicate in the form 'T x', or just 'T' if you don't care about the name",
                        stack,
                    ));
                }

                let matching_trait =
                    left[0]
                        .evaluate(env, stack)?
                        .get_or::<Trait>("Expected trait", env, stack)?;

                let name = left[1].get_or::<Name>("Expected name", env, stack)?.name;

                (matching_trait, Some(name))
            }
        };

        let (derived_trait, derived_value) = if matches!(right, VariadicOperatorInput::Single(_))
            || matches!(&right, VariadicOperatorInput::List(items) if items.len() != 2)
        {
            return Err(Return::error(
                "Expected a value to derive and its trait",
                stack,
            ));
        } else if let VariadicOperatorInput::List(right) = right {
            let derived_trait =
                right[0]
                    .evaluate(env, stack)?
                    .get_or::<Trait>("Expected trait", env, stack)?;

            let derived_value = right[1].clone();

            (derived_trait, derived_value)
        } else {
            unreachable!()
        };

        let derive_env = Environment::child_of(env).into_ref();

        env.borrow_mut()
            .add_conformance(matching_trait, derived_trait, move |value, _, stack| {
                if let Some(name) = &name {
                    derive_env.borrow_mut().set_variable(name, value.clone());
                }

                derived_value.evaluate(&derive_env, stack)
            });

        Ok(Value::empty())
    });

    add_variadic_operator(&conformance_operator, &assignment_precedence_group);

    env.borrow_mut()
        .set_variable("==", Value::of(Operator::Variadic(conformance_operator)));

    // Template operator (=>)

    let function_precedence_group = add_variadic_precedence_group(
        Associativity::Right,
        VariadicPrecedenceGroupComparison::lower_than(&assignment_precedence_group),
    );

    let template_operator = VariadicOperator::collect(|parameter, value_to_expand, env, stack| {
        let name = match parameter {
            VariadicOperatorInput::Single(parameter) => {
                parameter.get_or::<Name>("Template parameter must be a name", env, stack)?
            }
            _ => return Err(Return::error("Expected template parameter", stack)),
        };

        Ok(Value::of(Template {
            parameter: name.name,
            replace_in: value_to_expand.into_value(),
        }))
    });

    add_variadic_operator(&template_operator, &function_precedence_group);

    env.borrow_mut()
        .set_variable("=>", Value::of(Operator::Variadic(template_operator)));

    // Closure operator (->)

    let closure_operator = VariadicOperator::collect(|parameter, return_value, env, stack| {
        let (validation, parameter) = match parameter {
            VariadicOperatorInput::Single(parameter) => {
                let parameter =
                    parameter.get_or::<Name>("Closure parameter must be a name", env, stack)?;

                (Validation::any(), parameter)
            }
            VariadicOperatorInput::List(parameter) if parameter.len() == 2 => {
                let validation = parameter[0].evaluate(env, stack)?.get_or::<Validation>(
                    "Expected validation",
                    env,
                    stack,
                )?;

                let parameter =
                    parameter[1].get_or::<Name>("Closure parameter must be a name", env, stack)?;

                (validation, parameter)
            }
            _ => return Err(Return::error("Expected closure parameter", stack)),
        };

        let captured_env = env.clone();

        Ok(Value::of(Closure {
            captured_env,
            validation,
            parameter,
            return_value: return_value.into_value(),
        }))
    });

    add_variadic_operator(&closure_operator, &function_precedence_group);

    env.borrow_mut()
        .set_variable("->", Value::of(Operator::Variadic(closure_operator)));

    // 'for' operator

    let for_precedence_group = add_binary_precedence_group(
        Associativity::Right,
        BinaryPrecedenceGroupComparison::lowest(),
    );

    let for_operator = BinaryOperator::collect(|left, right, env, stack| {
        let validation =
            left.evaluate(env, stack)?
                .get_or::<Validation>("Expected validation", env, stack)?;

        let value = right.evaluate(env, stack)?;

        match validation(&value, env, stack)? {
            Validated::Valid(value) => Ok(value),
            Validated::Invalid => Err(Return::error("Value does not satisfy validation", stack)),
        }
    });

    add_binary_operator(&for_operator, &for_precedence_group);

    env.borrow_mut()
        .set_variable("for", Value::of(Operator::Binary(for_operator)));

    // 'as' operator (equivalent to 'T (T for x)')

    let as_precedence_group = add_binary_precedence_group(
        Associativity::Left,
        BinaryPrecedenceGroupComparison::lowest(),
    );

    let as_operator = BinaryOperator::collect(|value, r#trait, env, stack| {
        let r#trait =
            r#trait
                .evaluate(env, stack)?
                .get_or::<Trait>("Expected trait", env, stack)?;

        let trait_value = value.evaluate(env, stack)?.get_trait_or(
            &r#trait,
            "Value does not have trait",
            env,
            stack,
        )?;

        Ok(trait_value)
    });

    add_binary_operator(&as_operator, &as_precedence_group);

    env.borrow_mut()
        .set_variable("as", Value::of(Operator::Binary(as_operator)));

    // Math

    macro_rules! math {
        ($operation:tt, $precedence_group:ident) => {
            math!($operation, $precedence_group, |_, _, _| Ok(()))
        };
        ($operation:tt, $precedence_group:ident, $check:expr) => {{
            let operator = BinaryOperator::collect(|left, right, env, stack| {
                let left = left
                    .evaluate(env, stack)?
                    .get::<Number>(env, stack)?
                    .number;

                let right = right
                    .evaluate(env, stack)?
                    .get::<Number>(env, stack)?
                    .number;

                $check(&left, &right, stack)?;

                let result = left $operation right;

                Ok(Value::of(Number::new(result)))
            });

            add_binary_operator(&operator, &$precedence_group);

            env.borrow_mut().set_variable(
                stringify!($operation),
                Value::of(Operator::Binary(operator)),
            );
        }};
    }

    let addition_precedence_group = add_binary_precedence_group(
        Associativity::Left,
        BinaryPrecedenceGroupComparison::lowest(),
    );

    math!(+, addition_precedence_group);
    math!(-, addition_precedence_group);

    let multiplication_precedence_group = add_binary_precedence_group(
        Associativity::Left,
        BinaryPrecedenceGroupComparison::higher_than(&addition_precedence_group),
    );

    math!(*, multiplication_precedence_group);

    math!(
        /,
        multiplication_precedence_group,
        |_, right: &f64, stack| if *right == 0.0 {
            Err(Return::error("Cannot divide by 0", stack))
        } else {
            Ok(())
        }
    );

    macro_rules! boolean_math {
        ($name:expr, $operation:expr, $precedence_group:ident, $prelude_env:expr) => {{
            let stdlib_env = env.clone();

            let operator = BinaryOperator::collect(move |left, right, env, stack| {
                let left = left.evaluate(env, stack)?.get::<Number>(env, stack)?;

                let right = right.evaluate(env, stack)?.get::<Number>(env, stack)?;

                let result = $operation(left.number, right.number);

                // This will always work because 'true' and 'false' are defined
                // inside the standard library, which we have full control over
                let value = Name::new(&result.to_string())
                    .resolve(&stdlib_env, stack)
                    .expect("'true' and 'false' somehow aren't defined");

                Ok(value)
            });

            add_binary_operator(&operator, &$precedence_group);

            env.borrow_mut()
                .set_variable($name, Value::of(Operator::Binary(operator)));
        }};
    }

    let comparison_precedence_group = add_binary_precedence_group(
        Associativity::Right,
        BinaryPrecedenceGroupComparison::lower_than(&addition_precedence_group),
    );

    boolean_math!(">", |a, b| a > b, comparison_precedence_group, env);
    boolean_math!("<", |a, b| a < b, comparison_precedence_group, env);
    boolean_math!(
        "=",
        |a: f64, b: f64| (a - b).abs() < f64::EPSILON,
        comparison_precedence_group,
        env
    );

    // 'format' function

    env.borrow_mut().set_variable(
        "format",
        Value::of(Function::new(|value, env, stack| {
            let format_text = value.get_or::<Text>("Expected format text", env, stack)?;

            fn build_formatter(remaining_strings: Vec<String>, result: String) -> Value {
                if remaining_strings.len() == 1 {
                    Value::of(Text::new(&(result + &remaining_strings[0])))
                } else {
                    Value::of(Function::new(move |value, env, stack| {
                        let (leading_string, remaining_strings) =
                            remaining_strings.split_first().unwrap();

                        let value = value.evaluate(env, stack)?;

                        let text = value.get_or::<Text>(
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
    );

    // Global environment functions

    env.borrow_mut().set_variable(
        "inline-global!",
        Value::of(Function::new(|value, env, stack| {
            let block = value.get_or::<Block>("Expected block", env, stack)?;
            block.reduce_inline(&Environment::global(), stack)
        })),
    );

    env.borrow_mut().set_variable(
        "use-global!",
        Value::of(Function::new(|value, env, stack| {
            let block =
                value
                    .evaluate(env, stack)?
                    .get_or::<Module>("Expected module", env, stack)?;

            Environment::global()
                .borrow_mut()
                .r#use(&block.env.borrow());

            Ok(Value::empty())
        })),
    );

    env.borrow_mut().set_computed_variable("global?", {
        let stdlib_env = env.clone();

        move |env, stack| {
            let is_global = Environment::is_global(env);

            // This will always work; see above
            let value = Name::new(&is_global.to_string())
                .resolve(&stdlib_env, stack)
                .expect("'true' and 'false' somehow aren't defined");

            Ok(value)
        }
    });

    // 'evaluate-text!' function

    fn evaluate_text(code: &str, env: &EnvironmentRef, stack: &Stack) -> Result {
        let (tokens, lookup) = wipple_parser::lex(&code);

        let ast = wipple_parser::parse_inline_program(&mut tokens.iter().peekable(), &lookup)
            .map_err(|error| {
                wipple::Return::error(&format!("Error parsing: {}", error.message), stack)
            })?;

        let program = wipple_parser::convert(&ast, None);

        program.evaluate(&env, stack)
    }

    env.borrow_mut().set_variable(
        "evaluate-text!",
        Value::of(Function::new(|value, env, stack| {
            let code = value.get_or::<Text>("Expected text", env, stack)?.text;

            evaluate_text(&code, env, stack)
        })),
    );

    // Dot operator (.) -- 'a . b' is equivalent to 'b a'

    let dot_precedence_group = add_binary_precedence_group(
        Associativity::Left,
        BinaryPrecedenceGroupComparison::lowest(),
    );

    let dot_operator = BinaryOperator::collect(|left, right, env, stack| {
        right.evaluate(env, stack)?.call(left, env, stack)
    });

    add_binary_operator(&dot_operator, &dot_precedence_group);

    env.borrow_mut()
        .set_variable(".", Value::of(Operator::Binary(dot_operator)));

    // Flow operator (|) -- 'a | b' is equivalent to 'x -> b (a x)'

    let flow_precedence_group = add_binary_precedence_group(
        Associativity::Left,
        BinaryPrecedenceGroupComparison::lowest(),
    );

    let flow_operator = BinaryOperator::collect(|left, right, env, stack| {
        let left =
            left.evaluate(env, stack)?
                .get_or::<Function>("Expected function", env, stack)?;

        let right =
            right
                .evaluate(env, stack)?
                .get_or::<Function>("Expected function", env, stack)?;

        Ok(Value::of(Function::new(move |value, env, stack| {
            right(left(value, env, stack)?, env, stack)
        })))
    });

    add_binary_operator(&flow_operator, &flow_precedence_group);

    env.borrow_mut()
        .set_variable("|", Value::of(Operator::Binary(flow_operator)));
}
