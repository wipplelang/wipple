use std::path::PathBuf;

use crate::*;
use rust_embed::RustEmbed;
use wipple::*;

#[ext(pub, name = EnvStdlibExt)]
impl Env {
    fn stdlib(stack: &Stack) -> Result<Env> {
        Env::try_with(|env| {
            env.r#use(&Env::builtins(stack)?, stack)?;
            load_prelude(env, stack)?;
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

        {
            let path = PathBuf::from(&*path);

            let mut stack = stack.clone();
            // This path is only used for diagnostics, because these files can't
            // import other files
            *stack.current_file_mut() = CurrentFile(Some(path.clone()));

            include_string(code, Some(path), &file_env, &stack)?;
        }

        env.r#use(&file_env, stack)?;
    }

    Ok(())
}

fn load_prelude(env: &Env, stack: &Stack) -> Result<()> {
    env.set_variable(stack, "#stdlib-link", stdlib_link_function())?;

    env.set_variable(stack, "trait", trait_function())?;

    env.set_variable(stack, "is", is_function())?;

    env.set_variable(stack, "inline", inline_function())?;
    env.set_variable(stack, "new", new_function())?;
    env.set_variable(stack, "use", use_function())?;
    env.set_variable(stack, "eval-global!", eval_global_function())?;
    env.set_variable(stack, "use-global!", use_global_function())?;
    env.set_computed_variable(stack, "global?", globalq_computed_variable())?;

    env.set_variable(stack, "match", match_function())?;
    env.set_variable(stack, "loop", loop_function())?;
    env.set_variable(stack, "return", return_function())?;
    env.set_variable(stack, "break", break_function())?;

    env.set_variable(stack, "show", show_function())?;
    env.set_variable(stack, "format", format_function())?;

    let default_variadic_precedence_group: VariadicPrecedenceGroup =
        add_precedence_group!(Associativity::Left, lowest());

    let default_binary_precedence_group: BinaryPrecedenceGroup =
        add_precedence_group!(Associativity::Left, lowest());

    let assignment_precedence_group: VariadicPrecedenceGroup = add_precedence_group!(
        Associativity::Right,
        higher_than(default_variadic_precedence_group.clone()) // FIXME: Remove PrecedenceGroupTrait
    );

    env.add_operator(
        ":",
        assignment_operator(),
        &assignment_precedence_group,
        stack,
    )?;

    env.add_operator(
        ":>",
        computed_assignment_operator(),
        &assignment_precedence_group,
        stack,
    )?;

    env.add_operator(
        "==",
        relation_operator(),
        &assignment_precedence_group,
        stack,
    )?;

    let function_precedence_group = add_precedence_group!(
        Associativity::Right,
        lower_than(assignment_precedence_group),
    );

    env.add_operator("->", closure_operator(), &function_precedence_group, stack)?;
    env.add_operator("=>", template_operator(), &function_precedence_group, stack)?;

    let power_precedence_group: BinaryPrecedenceGroup = add_precedence_group!(
        Associativity::Left,
        higher_than(default_binary_precedence_group)
    );

    env.add_operator("^", power_operator(), &power_precedence_group, stack)?;

    let multiplication_precedence_group =
        add_precedence_group!(Associativity::Left, lower_than(power_precedence_group));

    env.add_operator(
        "*",
        multiply_operator(),
        &multiplication_precedence_group,
        stack,
    )?;

    env.add_operator(
        "/",
        divide_operator(),
        &multiplication_precedence_group,
        stack,
    )?;

    env.add_operator(
        "mod",
        modulo_operator(),
        &multiplication_precedence_group,
        stack,
    )?;

    let addition_precedence_group = add_precedence_group!(
        Associativity::Left,
        lower_than(multiplication_precedence_group)
    );

    env.add_operator("+", add_operator(), &addition_precedence_group, stack)?;
    env.add_operator("-", subtract_operator(), &addition_precedence_group, stack)?;

    let convert_precedence_group: BinaryPrecedenceGroup =
        add_precedence_group!(Associativity::Left, lower_than(addition_precedence_group));

    env.add_operator("as", as_operator(), &convert_precedence_group, stack)?;
    env.add_operator("as?", asq_operator(), &convert_precedence_group, stack)?;
    env.add_operator("is?", isq_operator(), &convert_precedence_group, stack)?;
    env.add_operator("into", into_operator(), &convert_precedence_group, stack)?;

    let comparison_precedence_group =
        add_precedence_group!(Associativity::Left, lower_than(convert_precedence_group));

    env.add_operator(
        "=",
        equal_to_operator(),
        &comparison_precedence_group,
        stack,
    )?;

    env.add_operator(
        "<",
        less_than_operator(),
        &comparison_precedence_group,
        stack,
    )?;

    let dot_precedence_group = add_precedence_group!(
        Associativity::Left,
        higher_than(default_variadic_precedence_group)
    );

    env.add_operator(".", dot_operator(), &dot_precedence_group, stack)?;

    let pipe_precedence_group =
        add_precedence_group!(Associativity::Left, lower_than(dot_precedence_group));

    env.add_operator("|", pipe_operator(), &pipe_precedence_group, stack)?;

    Ok(())
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
                    env.set_variable(stack, &name.name, matched_value)?;
                }

                return result.evaluate(&env, stack).map(Cow::into_owned);
            }

            Err(error("Value did not match any pattern", stack))
        })))
    }))
}

fn loop_function() -> Value {
    Value::of(Function::new(|value, env, stack| loop {
        if let Some(value) = catch_only!(Break in value.evaluate(env, stack))? {
            return Ok(value);
        }
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
    Value::of(Function::new(|value, env, stack| {
        let value = value.evaluate(env, stack)?;
        let format_text = value.get_or::<Text>("Expected format text", env, stack)?;

        fn build_formatter(index: usize, remaining_strings: Vec<String>, result: String) -> Value {
            if remaining_strings.len() == 1 {
                Value::of(Text::new(result + &remaining_strings[0]))
            } else {
                Value::of(Function::new(move |value, env, stack| {
                    let (leading_string, remaining_strings) =
                        remaining_strings.split_first().unwrap();

                    let value = value.evaluate(env, stack)?;

                    let text = value.get_or::<Text>(
                        &format!(
                            "Cannot format input #{} because it cannot be represented as text",
                            index,
                        ),
                        env,
                        stack,
                    )?;

                    Ok(build_formatter(
                        index - 1,
                        remaining_strings.to_vec(),
                        result.clone() + leading_string + &text.text,
                    ))
                }))
            }
        }

        let strings = format_text
            .text
            .split('_')
            .map(String::from)
            .collect::<Vec<_>>();

        Ok(build_formatter(strings.len(), strings, String::new()))
    }))
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

        let captured_env = env.child();

        env.add_relation(
            from_trait,
            to_trait,
            stack,
            DeriveValueFn::new(move |value, stack| {
                let env = captured_env.child();

                if let Some(name) = &name {
                    env.set_variable(stack, name, value)?;
                }

                derived_value.evaluate(&env, stack).map(Cow::into_owned)
            }),
        )?;

        Ok(Value::empty())
    })
}

// -> and => operators

fn closure_operator() -> VariadicOperator {
    VariadicOperator::new(|left, right, env, stack| {
        let (parameter_pattern, parameter_name) = get_parameter(left, env, stack)?;
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
    VariadicOperator::new(|left, right, env, stack| {
        let mut parameters = Vec::new();

        for parameter in Vec::from(left) {
            let (pattern, name) = get_parameter(parameter.into(), env, stack)?;
            parameters.push((pattern, name));
        }

        let captured_env = env.clone();

        Ok(Value::of(Template {
            captured_env,
            parameters,
            return_value: Value::from(right),
            partially_applied_inputs: Vec::new(),
        }))
    })
}

fn get_parameter(input: VariadicInput, env: &Env, stack: &Stack) -> Result<(Pattern, String)> {
    match input {
        VariadicInput::Single(input) => {
            let parameter = input
                .get_or::<Name>("Parameter must be a name", env, stack)?
                .name
                .clone();

            Ok((Pattern::any(), parameter))
        }
        VariadicInput::List(input) if input.len() == 2 => {
            let pattern = input[0]
                .evaluate(env, stack)?
                .get_or::<Pattern>("Expected pattern", env, stack)?
                .into_owned();

            let parameter = input[1]
                .get_or::<Name>("Parameter must be a name", env, stack)?
                .name
                .clone();

            Ok((pattern, parameter))
        }
        _ => Err(error("Expected parameter", stack)),
    }
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

fn dot_operator() -> VariadicOperator {
    VariadicOperator::new(|left, right, env, stack| {
        Value::from(right)
            .evaluate(env, stack)?
            .call_with(left.into(), env, stack)
    })
}

// | operator

fn pipe_operator() -> VariadicOperator {
    VariadicOperator::new(|left, right, env, stack| {
        let left = Value::from(left)
            .evaluate(env, stack)?
            .get_or::<Function>("Expected function", env, stack)?
            .into_owned();

        let right = Value::from(right)
            .evaluate(env, stack)?
            .get_or::<Function>("Expected function", env, stack)?
            .into_owned();

        Ok(Value::of(Function::new(move |value, env, stack| {
            right(left(value, env, stack)?, env, stack)
        })))
    })
}

// Math operators

macro_rules! make_math_operators {
    ($($operation:ident),* $(,)?) => {
        $(paste! {
            fn [<$operation _operator>]() -> BinaryOperator {
                BinaryOperator::new(|left, right, env, stack| {
                    left.evaluate(env, stack)?.$operation(right, env, stack)
                })
            }
        })*
    };
}

make_math_operators!(power, multiply, divide, modulo, add, subtract, equal_to, less_than);
