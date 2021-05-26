use crate::*;
use wipple::*;

#[derive(TypeInfo, Debug, Clone)]
pub struct List {
    pub items: Vec<Value>,
    pub location: Option<SourceLocation>,
}

impl Primitive for List {}

impl List {
    pub fn new(items: Vec<Value>) -> Self {
        List::new_located(items, None)
    }

    pub fn new_located(items: Vec<Value>, location: Option<SourceLocation>) -> Self {
        List {
            items: items.to_vec(),
            location,
        }
    }
}

pub(crate) fn setup(env: &Env, stack: &Stack) -> Result<()> {
    env.set_variable("List", Value::of(Trait::of::<List>()));

    // List == Evaluate
    env.add_relation_between(stack, |list: List| {
        EvaluateFn::new(move |env, stack| {
            let mut stack = stack.clone();
            stack
                .evaluation_mut()
                .queue_location(list.location.as_ref());

            let operators = list.find_operators(env, &stack)?;

            if let Some(parsed) = list.parse_operators(operators, env, &stack)? {
                return Ok(parsed);
            }

            // Reduce the list as a series of function calls

            let mut result = match list.items.first() {
                Some(value) => value.evaluate(env, &stack)?.into_owned(),
                None => {
                    // Empty list evaluates to itself
                    return Ok(Value::of(list.clone()));
                }
            };

            for item in list.items.clone().into_iter().skip(1) {
                result = result.call_with(item, env, &stack)?;
            }

            Ok(result)
        })
    })?;

    // List == Text
    env.add_relation_between_with(stack, |list: List, env, stack| {
        let mut items = String::new();

        let count = list.items.len();

        for (index, item) in list.items.into_iter().enumerate() {
            let text = item.format(env, stack)?;
            items.push_str(&text);

            if index != count - 1 {
                items.push(' ');
            }
        }

        Ok(Text::new(format!("({})", items)))
    })?;

    Ok(())
}
