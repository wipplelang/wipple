use crate::*;
use rust_embed::RustEmbed;
use wipple::*;

#[ext(pub, name = EnvStdlibExt)]
impl Env {
    fn stdlib(stack: &Stack) -> Result<Env> {
        Env::try_with(|env| {
            env.r#use(&Env::builtins(stack)?, stack)?;
            load_prelude(env);
            load_files(env, stack)?;

            Ok(())
        })
    }
}

fn load_files(env: &Env, stack: &Stack) -> Result<()> {
    #[derive(RustEmbed)]
    #[folder = "src"]
    struct StdlibFiles;

    for path in StdlibFiles::iter() {
        let file_env = env.child();

        let file = StdlibFiles::get(&path).unwrap();
        let code = std::str::from_utf8(&file).unwrap();

        include_string(code, Some(&path), &file_env, stack)?;

        env.r#use(&file_env, stack)?;
    }

    Ok(())
}

fn load_prelude(env: &Env) {
    env.set_variable("#stdlib-link", stdlib_link_function());

    env.set_variable("trait", trait_function());

    env.set_variable("is", is_function());

    env.set_variable("inline", inline_function());
    env.set_variable("new", new_function());
    env.set_variable("use", use_function());
    env.set_variable("eval-global!", eval_global_function());
    env.set_variable("use-global!", use_global_function());
    env.set_computed_variable("global?", globalq_computed_variable());

    env.set_variable("match", match_function());
    env.set_variable("return", return_function());
    env.set_variable("break", break_function());

    env.set_variable("show", show_function());
    env.set_variable("format", format_function());

    let assignment_precedence_group: VariadicPrecedenceGroup =
        add_precedence_group!(Associativity::Right, highest());

    env.add_operator(":", assignment_operator(), &assignment_precedence_group);
    env.add_operator(
        ":>",
        computed_assignment_operator(),
        &assignment_precedence_group,
    );
    env.add_operator("==", relation_operator(), &assignment_precedence_group);

    let function_precedence_group = add_precedence_group!(
        Associativity::Right,
        lower_than(assignment_precedence_group),
    );

    env.add_operator("->", closure_operator(), &function_precedence_group);
    env.add_operator("=>", template_operator(), &function_precedence_group);

    let convert_precedence_group: BinaryPrecedenceGroup =
        add_precedence_group!(Associativity::Left, lowest());

    env.add_operator("as", as_operator(), &convert_precedence_group);
    env.add_operator("as?", asq_operator(), &convert_precedence_group);
    env.add_operator("is?", isq_operator(), &convert_precedence_group);
    env.add_operator("into", into_operator(), &convert_precedence_group);

    let dot_precedence_group =
        add_precedence_group!(Associativity::Left, lower_than(convert_precedence_group));

    env.add_operator(".", dot_operator(), &dot_precedence_group);

    let pipe_precedence_group =
        add_precedence_group!(Associativity::Left, lower_than(dot_precedence_group));

    env.add_operator("|", pipe_operator(), &pipe_precedence_group);
}

// Link values to the standard library

fn stdlib_link_function() -> Value {
    Value::of(Function::new(|value, env, stack| {
        let name = value
            .get_or::<Name>("Expected name", env, stack)?
            .name
            .clone();

        let value = value.evaluate(env, stack)?.into_owned();

        set_linked_value(name, value);

        Ok(Value::empty())
    }))
}

// Trait functions

fn trait_function() -> Value {
    Value::of(Function::new(|value, env, stack| {
        let pattern = value
            .evaluate(env, stack)?
            .get_or::<Pattern>("Expected pattern", env, stack)?
            .into_owned();

        let r#trait = Trait::new(pattern);

        Ok(Value::of(r#trait))
    }))
}

// Pattern functions

fn is_function() -> Value {
    Value::of(Function::new(|value, env, stack| {
        let pattern = value
            .evaluate(env, stack)?
            .get_or::<Pattern>("Expected pattern", env, stack)?
            .into_owned();

        Ok(Value::of(Pattern::is(pattern)))
    }))
}

// Module functions

fn inline_function() -> Value {
    Value::of(Function::new(|value, env, stack| {
        let block = value.get_or::<Block>("Expected block", env, stack)?;
        block.reduce(env, stack)
    }))
}

fn new_function() -> Value {
    Value::of(Function::new(|value, env, stack| {
        let block = value.get_or::<Block>("Expected block", env, stack)?;
        EvaluateBlockFn::into_module()(&block, env, stack)
    }))
}

fn use_function() -> Value {
    Value::of(Function::new(|value, env, stack| {
        let value = value.evaluate(env, stack)?;
        let module = value.get_or::<Module>("Expected a module", env, stack)?;

        env.r#use(&module.env, stack)?;

        Ok(Value::empty())
    }))
}

fn use_global_function() -> Value {
    Value::of(Function::new(|value, env, stack| {
        let value = value.evaluate(env, stack)?;
        let block = value.get_or::<Module>("Expected module", env, stack)?;

        Env::global().r#use(&block.env, stack)?;

        Ok(Value::empty())
    }))
}

fn eval_global_function() -> Value {
    Value::of(Function::new(|value, _, stack| {
        value.evaluate(&Env::global(), stack).map(Cow::into_owned)
    }))
}

fn globalq_computed_variable() -> ComputeFn {
    ComputeFn::new(|env, _| Ok(Value::of(Variant::condition(env.is_global()))))
}

// Control flow

fn match_function() -> Value {
    Value::of(Function::new(|value, env, stack| {
        let value = value.evaluate(env, stack)?.into_owned();

        Ok(Value::of(Function::new(move |block, env, stack| {
            let block = block.get_or::<Block>("Expected block", env, stack)?;
            let patterns = Rc::new(RefCell::new(Some(Vec::new())));

            let match_env = env.child();

            match_env.set_assignment(AssignmentFn::new({
                let patterns = patterns.clone();
                move |left, right, env, stack| {
                    let (pattern, name) = match left {
                        VariadicInput::Single(value) => {
                            let pattern = value
                                .evaluate(env, stack)?
                                .get_or::<Pattern>("Expected pattern", env, stack)?
                                .into_owned();

                            (pattern, None)
                        }
                        VariadicInput::List(items) if items.len() == 2 => {
                            let pattern = items[0]
                                .evaluate(env, stack)?
                                .get_or::<Pattern>("Expected pattern", env, stack)?
                                .into_owned();

                            let name = items[1]
                                .get_or::<Name>("Expected name", env, stack)?
                                .into_owned();

                            (pattern, Some(name))
                        }
                        _ => {
                            return Err(error("Expected pattern to match and optional name", stack))
                        }
                    };

                    let right = Value::from(right);

                    patterns
                        .borrow_mut()
                        .as_mut()
                        .unwrap()
                        .push((pattern, name, right));

                    Ok(())
                }
            }));

            block.reduce(&match_env, stack)?;

            let patterns = patterns.take().unwrap();

            for (pattern, name, result) in patterns {
                let matched_value = match pattern(&value, env, stack)? {
                    Some(value) => value.into_owned(),
                    None => continue,
                };

                let env = env.child();

                if let Some(name) = name {
                    env.set_variable(&name.name, matched_value);
                }

                return result.evaluate(&env, stack).map(Cow::into_owned);
            }

            Err(error("Value did not match any pattern", stack))
        })))
    }))
}

fn return_function() -> Value {
    Value::of(Function::transparent(|value, env, stack| {
        let value = value.evaluate(env, stack)?;
        Err(Exit::Return(value.into_owned(), stack.clone()))
    }))
}

fn break_function() -> Value {
    Value::of(Function::transparent(|value, env, stack| {
        let value = value.evaluate(env, stack)?;
        Err(Exit::Break(value.into_owned(), stack.clone()))
    }))
}

// Output functions

stored_closure!(pub struct ShowFn(Value, &Env, &Stack) -> Result<()>);

impl Default for ShowFn {
    fn default() -> Self {
        ShowFn::new(|_, _, stack| {
            Err(error(
                "Cannot use 'show' because this runtime does not handle output",
                stack,
            ))
        })
    }
}

stack_key!(pub show: ShowFn);

fn show_function() -> Value {
    Value::of(Function::new(|value, env, stack| {
        stack.show()(value, env, stack)?;
        Ok(Value::empty())
    }))
}

fn format_function() -> Value {
    Value::of(Function::new(|_, _, _| todo!()))
}

// :, :> and == operators

macro_rules! make_assignment_operator {
    ($assign:ident) => {
        VariadicOperator::new(|left, right, env, stack| {
            env.$assign()(left, right, env, stack)?;
            Ok(Value::empty())
        })
    };
}

fn assignment_operator() -> VariadicOperator {
    make_assignment_operator!(assignment)
}

fn computed_assignment_operator() -> VariadicOperator {
    make_assignment_operator!(computed_assignment)
}

fn relation_operator() -> VariadicOperator {
    VariadicOperator::new(|left, right, env, stack| {
        let (from_trait, name) = match left {
            VariadicInput::Single(left) => {
                let from_trait = left
                    .evaluate(env, stack)?
                    .get_or::<Trait>("Expected trait", env, stack)?
                    .into_owned();

                (from_trait, None)
            }
            VariadicInput::List(left) => {
                if left.len() != 2 {
                    return Err(error(
                        "Expected relation predicate in the form 'x T', or just 'T' if you don't care about the name",
                        stack,
                    ));
                }

                let from_trait = left[0]
                    .evaluate(env, stack)?
                    .get_or::<Trait>("Expected trait", env, stack)?
                    .into_owned();

                let name = left[1]
                    .get_or::<Name>("Expected name", env, stack)?
                    .name
                    .clone();

                (from_trait, Some(name))
            }
        };

        let (to_trait, derived_value) = match right {
            VariadicInput::List(right) if right.len() == 2 => {
                let derived_trait = right[0]
                    .evaluate(env, stack)?
                    .get_or::<Trait>("Expected trait", env, stack)?
                    .into_owned();

                let derived_value = right[1].clone();

                (derived_trait, derived_value)
            }
            _ => return Err(error("Expected a value to derive and its trait", stack)),
        };

        let derive_env = env.child();

        env.add_relation(
            from_trait,
            to_trait,
            stack,
            DeriveValueFn::new(move |value, _, stack| {
                if let Some(name) = &name {
                    derive_env.set_variable(name, value);
                }

                derived_value
                    .evaluate(&derive_env, stack)
                    .map(Cow::into_owned)
            }),
        )?;

        Ok(Value::empty())
    })
}

// -> and => operators

fn closure_operator() -> VariadicOperator {
    VariadicOperator::new(|left, right, env, stack| {
        let (parameter_pattern, parameter_name) = match left {
            VariadicInput::Single(input) => {
                let parameter = input
                    .get_or::<Name>("Closure parameter must be a name", env, stack)?
                    .name
                    .clone();

                (Pattern::any(), parameter)
            }
            VariadicInput::List(input) if input.len() == 2 => {
                let pattern = input[0]
                    .evaluate(env, stack)?
                    .get_or::<Pattern>("Expected pattern", env, stack)?
                    .into_owned();

                let parameter = input[1]
                    .get_or::<Name>("Closure parameter must be a name", env, stack)?
                    .name
                    .clone();

                (pattern, parameter)
            }
            _ => return Err(error("Expected closure parameter", stack)),
        };

        let captured_env = env.clone();

        Ok(Value::of(Closure {
            captured_env,
            parameter_pattern,
            parameter_name,
            return_value: Value::from(right),
        }))
    })
}

fn template_operator() -> VariadicOperator {
    VariadicOperator::new(|_, _, _, _| todo!())
}

// as, as?, is? and into operators

fn as_operator() -> BinaryOperator {
    BinaryOperator::new(|left, right, env, stack| {
        let left = left.evaluate(env, stack)?;
        let right = right.evaluate(env, stack)?;

        let pattern = right.get_or::<Pattern>("Expected pattern", env, stack)?;

        pattern(&left, env, stack)?
            .ok_or_else(|| error("Invalid value", stack))
            .map(Cow::into_owned)
    })
}

fn asq_operator() -> BinaryOperator {
    BinaryOperator::new(|left, right, env, stack| {
        let value = left.evaluate(env, stack)?;
        let right = right.evaluate(env, stack)?;

        let pattern = right.get_or::<Pattern>("Expected pattern", env, stack)?;
        let matched = pattern(&value, env, stack)?.map(Cow::into_owned);

        Ok(Value::of(Variant::maybe(matched)))
    })
}

fn isq_operator() -> BinaryOperator {
    BinaryOperator::new(|left, right, env, stack| {
        let left = left.evaluate(env, stack)?;
        let right = right.evaluate(env, stack)?;

        let pattern = right.get_or::<Pattern>("Expected pattern", env, stack)?;
        let valid = pattern(&left, env, stack)?.is_some();

        Ok(Value::of(Variant::condition(valid)))
    })
}

fn into_operator() -> BinaryOperator {
    BinaryOperator::new(|left, right, env, stack| {
        let value = left.evaluate(env, stack)?;
        let right = right.evaluate(env, stack)?;

        let r#trait = right
            .get_or::<Trait>("Expected trait", env, stack)?
            .into_owned();

        let trait_value = value
            .get_trait_or(
                &r#trait,
                "Cannot use this value to represent this trait",
                env,
                stack,
            )?
            .into_owned();

        Ok(Value::new(r#trait, trait_value))
    })
}

// . operator

fn dot_operator() -> BinaryOperator {
    BinaryOperator::new(|left, right, env, stack| {
        right.evaluate(env, stack)?.call_with(left, env, stack)
    })
}

// | operator

fn pipe_operator() -> BinaryOperator {
    BinaryOperator::new(|left, right, env, stack| {
        let left = left
            .evaluate(env, stack)?
            .get_or::<Function>("Expected function", env, stack)?
            .into_owned();

        let right = right
            .evaluate(env, stack)?
            .get_or::<Function>("Expected function", env, stack)?
            .into_owned();

        Ok(Value::of(Function::new(move |value, env, stack| {
            right(left(value, env, stack)?, env, stack)
        })))
    })
}
