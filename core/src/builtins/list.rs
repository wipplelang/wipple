use crate::*;

#[derive(Clone)]
pub struct List {
    pub items: Vec<Value>,
    pub location: Option<SourceLocation>,
}

fundamental_primitive!(list for List);

pub(crate) fn setup(env: &mut Environment) {
    env.add_primitive_conformance(|list: List| {
        EvaluateFn::new(move |env, stack| {
            let mut stack = stack.clone();
            if let Some(location) = &list.location {
                stack.queue_location(location);
            }

            let operators = list.find_operators(env, &stack)?;

            if let Some(parsed) = list.parse_operators(operators, env, &stack)? {
                return Ok(parsed);
            }

            // Reduce the list as a series of function calls

            let mut result = match list.items.first() {
                Some(value) => value.evaluate(env, &stack)?,
                None => {
                    // Empty list evaluates to itself
                    return Ok(Value::of(list.clone()));
                }
            };

            for item in list.items.iter().skip(1) {
                result = result.call(item, env, &stack)?;
            }

            Ok(result)
        })
    });

    env.add_primitive_conformance(|list: List| {
        MacroExpandFn::new(move |parameter, replacement, env, stack| {
            let mut expanded_items = vec![];

            for item in &list.items {
                let item = item.macro_expand(parameter, replacement, env, stack)?;

                expanded_items.push(item);
            }

            Ok(Value::of(List {
                items: expanded_items,
                location: None,
            }))
        })
    });

    env.add_conformance(TraitID::text(), |value, env, stack| {
        let list = match value.get_primitive_if_present::<List>(env, stack)? {
            Some(list) => list,
            None => return Ok(None),
        };

        let mut items = Vec::new();

        for item in &list.items {
            let text = item.format(env, stack)?;
            items.push(text);
        }

        Ok(Some(Value::of(Text {
            text: format!("({})", items.join(" ")),
            location: None,
        })))
    });
}
