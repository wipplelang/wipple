#[macro_use]
pub mod fundamentals;
pub mod builtins;

pub use builtins::*;
pub use fundamentals::*;

use num_rational::BigRational;
use std::rc::Rc;

pub fn init(env: &mut Environment) {
    builtins::init(env);

    // TODO: TEMPORARY (implement in Wipple code)

    // 'new' function

    fn add(
        base: Value,
        trait_constructor: TraitConstructor,
        value: Value,
        env: &mut Environment,
        stack: &ProgramStack,
    ) -> Result {
        let value = match trait_constructor
            .validation
            .validate(&value, env, stack)?
            .unwrap()
        {
            ValidationResult::Valid(value) => value,
            ValidationResult::Invalid => {
                return Err(ProgramError::new(
                    "Cannot use this value to represent this trait",
                    stack,
                ))
            }
        };

        let r#trait = Trait::new(trait_constructor.id, move |_, _| Ok(value.clone()));

        Ok(base.add(r#trait))
    }

    env.variables.insert(
        String::from("new"),
        Value::new(Trait::function(Function::new(|input, env, stack| {
            let trait_constructor = input.get_trait(TraitID::trait_constructor, env, stack)?;

            Ok(Value::new(Trait::function(Function::new(
                move |input, env, stack| {
                    add(Value::empty(), trait_constructor.clone(), input, env, stack)
                },
            ))))
        }))),
    );

    // 'do' function

    env.variables.insert(
        String::from("do"),
        Value::new(Trait::function(Function::new(|input, env, stack| {
            let mut inner_env = env.clone();

            input.evaluate(&mut inner_env, stack)
        }))),
    );

    // Assignment operator (::)

    fn group(list: Vec<ListItem>) -> Value {
        if list.len() == 1 {
            list[0].value.clone()
        } else {
            Value::new(Trait::list(List {
                items: list,
                location: None,
            }))
        }
    };

    fn assign(
        left: Vec<ListItem>,
        right: impl Fn(&mut Environment, &ProgramStack) -> Result + 'static,
        env: &mut Environment,
        stack: &ProgramStack,
    ) -> Result {
        let left = group(left);

        let right = right(env, stack)?.evaluate(env, stack)?;

        let stack = stack.add(|| {
            format!(
                "Assigning '{}' to '{}'",
                right.format(env, stack),
                left.format(env, stack)
            )
        });

        let assign = left.get_trait_or(
            TraitID::assign,
            "Cannot assign to this value because it does not have the Assign trait",
            env,
            &stack,
        )?;

        assign.0(right, env, &stack)?;

        Ok(Value::empty())
    };

    let assignment_precedence_group = env.precedence_group(
        Associativity::Right,
        PrecedenceGroupComparison::<VariadicPrecedenceGroup>::highest(),
    );

    let assignment_operator = VariadicOperator::collect(|left, right, env, stack| {
        assign(left, move |_, _| Ok(group(right.clone())), env, stack)
    });

    env.add_variadic_operator(&assignment_operator, &assignment_precedence_group);
    env.variables.insert(
        String::from(":"),
        Value::new(Trait::operator(Operator::Variadic(assignment_operator))),
    );

    // Trait operator (::)

    let trait_operator = VariadicOperator::collect(|left, right, env, stack| {
        assign(
            left.clone(),
            move |env, stack| {
                let value = group(left.clone());

                // TODO: Auto-derive a value for the trait using the conformance
                // if only the trait is provided
                if right.len() != 2 {
                    return Err(ProgramError::new(
                        "Expected a trait and a value for the trait",
                        &stack,
                    ));
                }

                let trait_constructor_value = right[0].value.evaluate(env, stack)?;
                let trait_value = right[1].value.evaluate(env, stack)?;

                let stack = stack.add(|| {
                    format!(
                        "Adding trait '{}' with '{}' to '{}'",
                        trait_constructor_value.format(env, stack),
                        trait_value.format(env, stack),
                        value.format(env, stack)
                    )
                });

                let trait_constructor =
                    trait_constructor_value.get_trait(TraitID::trait_constructor, env, &stack)?;

                let value = add(value, trait_constructor, trait_value, env, &stack)?;

                Ok(value)
            },
            env,
            stack,
        )
    });

    env.add_variadic_operator(&trait_operator, &assignment_precedence_group);
    env.variables.insert(
        String::from("::"),
        Value::new(Trait::operator(Operator::Variadic(trait_operator.clone()))),
    );

    // Macro operator (=>)

    let function_precedence_group = env.precedence_group(
        Associativity::Right,
        PrecedenceGroupComparison::<VariadicPrecedenceGroup>::lower_than(
            assignment_precedence_group,
        ),
    );

    let macro_operator = VariadicOperator::collect(|left, right, env, stack| {
        let define_parameter = group(left).get_trait_or(
            TraitID::macro_parameter,
            "Macro parameter must have the Macro-Parameter trait",
            env,
            stack,
        )?;

        Ok(Value::new(Trait::r#macro(Macro {
            define_parameter,
            value_to_expand: group(right),
        })))
    });

    env.add_variadic_operator(&macro_operator, &function_precedence_group);
    env.variables.insert(
        String::from("=>"),
        Value::new(Trait::operator(Operator::Variadic(macro_operator.clone()))),
    );

    // Closures

    #[derive(Clone)]
    struct DefineClosureParameterFn(
        Rc<dyn Fn(Value, &mut Environment, &ProgramStack) -> Result<()>>,
    );

    let closure_parameter_trait_id =
        TraitID::<DefineClosureParameterFn>::builtin("Closure-Parameter");

    // Name ::= Closure-Parameter
    env.add_conformance(Conformance::new(
        closure_parameter_trait_id.clone(),
        TraitID::name.validation(),
        |name, _, _| {
            let name = name.clone();

            Ok(DefineClosureParameterFn(Rc::new(
                move |input, env, stack| {
                    let input = input.evaluate(env, stack)?;
                    env.variables.insert(name.0.clone(), input);
                    Ok(())
                },
            )))
        },
    ));

    // Quoted ::= Closure-Parameter
    env.add_conformance(Conformance::new(
        closure_parameter_trait_id.clone(),
        TraitID::quoted
            .validation()
            .and(Validation::new(|quoted: Quoted, env, stack| {
                TraitID::assign
                    .validation()
                    .validate(quoted.value, env, stack)
            })),
        |assign, _, _| {
            let assign = assign.clone();

            Ok(DefineClosureParameterFn(Rc::new(
                move |input, env, stack| {
                    assign.0(
                        Value::new(Trait::quoted(Quoted {
                            value: input,
                            location: None,
                        })),
                        env,
                        stack,
                    )
                },
            )))
        },
    ));

    // TODO: Closure trait

    let closure_operator = VariadicOperator::collect(move |left, right, env, stack| {
        let define_parameter = group(left).get_trait_or(
            closure_parameter_trait_id.clone(),
            "Closure parameter must have the Closure-Parameter trait",
            env,
            stack,
        )?;

        let return_value = group(right);

        let closure_env = env.clone();

        Ok(Value::new(Trait::function(Function::new(
            move |input, _, stack| {
                let mut closure_env = closure_env.clone();

                define_parameter.0(input, &mut closure_env, stack)?;

                return_value.evaluate(&mut closure_env, stack)
            },
        ))))
    });

    env.add_variadic_operator(&closure_operator, &function_precedence_group);
    env.variables.insert(
        String::from("->"),
        Value::new(Trait::operator(Operator::Variadic(closure_operator))),
    );

    // Math (temporary)

    // TODO: Implement using traits
    fn math(
        operation: impl Fn(BigRational, BigRational) -> BigRational + 'static,
    ) -> BinaryOperator {
        BinaryOperator::collect(move |left, right, env, stack| {
            let left = left
                .value
                .evaluate(env, stack)?
                .get_trait(TraitID::number, env, stack)?;

            let right = right
                .value
                .evaluate(env, stack)?
                .get_trait(TraitID::number, env, stack)?;

            let result = Number(operation(left.0, right.0));

            Ok(Value::new(Trait::number(result)))
        })
    }

    let addition_precedence_group = env.precedence_group(
        Associativity::Left,
        PrecedenceGroupComparison::<BinaryPrecedenceGroup>::lowest(),
    );

    let addition_operator = math(std::ops::Add::add);
    env.add_binary_operator(&addition_operator, &addition_precedence_group);
    env.variables.insert(
        String::from("+"),
        Value::new(Trait::operator(Operator::Binary(addition_operator))),
    );

    let subtraction_operator = math(std::ops::Sub::sub);
    env.add_binary_operator(&subtraction_operator, &addition_precedence_group);
    env.variables.insert(
        String::from("-"),
        Value::new(Trait::operator(Operator::Binary(subtraction_operator))),
    );

    let multiplication_precedence_group = env.precedence_group(
        Associativity::Left,
        PrecedenceGroupComparison::<BinaryPrecedenceGroup>::higher_than(addition_precedence_group),
    );

    let multiplication_operator = math(std::ops::Mul::mul);
    env.add_binary_operator(&multiplication_operator, &multiplication_precedence_group);
    env.variables.insert(
        String::from("*"),
        Value::new(Trait::operator(Operator::Binary(multiplication_operator))),
    );

    let division_operator = math(std::ops::Div::div);
    env.add_binary_operator(&division_operator, &multiplication_precedence_group);
    env.variables.insert(
        String::from("/"),
        Value::new(Trait::operator(Operator::Binary(division_operator))),
    );
}

#[cfg(test)]
mod test {
    use crate::*;

    #[test]
    fn test_env() -> Result<()> {
        use crate::*;

        let mut env = Environment::default();
        let stack = ProgramStack::new();

        init(&mut env);

        let block = Value::new(Trait::block(Block {
            statements: vec![List {
                items: vec![
                    ListItem {
                        value: Value::new(Trait::name(Name(String::from("f")))),
                        location: None,
                    },
                    ListItem {
                        value: Value::new(Trait::name(Name(String::from(":")))),
                        location: None,
                    },
                    ListItem {
                        value: Value::new(Trait::name(Name(String::from("f")))),
                        location: None,
                    },
                ],
                location: None,
            }],
            location: None,
        }));

        let result =
            block
                .evaluate(&mut env, &stack)?
                .get_trait(TraitID::text, &mut env, &stack)?;

        println!("{}", result.0);

        Ok(())
    }
}
