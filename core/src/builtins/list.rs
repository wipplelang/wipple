use crate::*;

#[derive(Clone)]
pub struct List {
    pub items: Vec<Value>,
    pub location: Option<SourceLocation>,
}

impl List {
    pub fn new(items: &[Value]) -> Self {
        List::new_located(items, None)
    }

    pub fn new_located(items: &[Value], location: Option<SourceLocation>) -> Self {
        List {
            items: items.to_vec(),
            location,
        }
    }
}

core_primitive!(pub list for List);

pub(crate) fn setup(env: &mut Environment) {
    // List == Evaluate
    env.add_primitive_conformance(|list: List| {
        EvaluateFn::new(move |env, stack| {
            let stack = match &list.location {
                Some(location) => stack.update_evaluation(|e| e.queue_location(&location)),
                None => stack,
            };

            let operators = list.find_operators(env, stack.clone())?;

            if let Some(parsed) = list.parse_operators(operators, env, stack.clone())? {
                return Ok(parsed);
            }

            // Reduce the list as a series of function calls

            let mut result = match list.items.first() {
                Some(value) => value.evaluate(env, stack.clone())?,
                None => {
                    // Empty list evaluates to itself
                    return Ok(Value::of(list.clone()));
                }
            };

            for item in list.items.iter().skip(1) {
                result = result.call(item, env, stack.clone())?;
            }

            Ok(result)
        })
    });

    // List == Replace-In-Template
    env.add_primitive_conformance(|list: List| {
        ReplaceInTemplateFn::new(move |parameter, replacement, env, stack| {
            let mut expanded_items = vec![];

            for item in &list.items {
                let item = item.replace_in_template(parameter, replacement, env, stack.clone())?;

                expanded_items.push(item);
            }

            Ok(Value::of(List::new(&expanded_items)))
        })
    });

    // List == Text
    env.add_conformance(Trait::list(), Trait::text(), |value, env, stack| {
        let list = value.clone().into_primitive::<List>();

        let mut items = Vec::new();

        for item in &list.items {
            let text = item.format(env, stack.clone())?;
            items.push(text);
        }

        Ok(Value::of(Text::new(&format!("({})", items.join(" ")))))
    });
}
