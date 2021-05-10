use crate::*;

#[derive(TypeInfo, Debug, Clone)]
pub struct List {
    pub items: Vec<Value>,
    pub location: Option<SourceLocation>,
}

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

pub(crate) fn setup(env: &mut EnvironmentInner) {
    env.set_variable("List", Value::of(Trait::of::<List>()));

    // List == Evaluate
    env.add_primitive_relation(|list: List| {
        EvaluateFn::new(move |env, stack| {
            let mut stack = stack.clone();
            stack.evaluation_mut().queue_location(&list.location);

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

            for item in list.items.clone().into_iter().skip(1) {
                result = result.call(item, env, &stack)?;
            }

            Ok(result)
        })
    });

    // List == Replace-In-Template
    env.add_primitive_relation(|list: List| {
        ReplaceInTemplateFn::new(move |parameter, replacement, env, stack| {
            let mut expanded_items = vec![];

            for item in &list.items {
                let item = item.replace_in_template(parameter, replacement, env, stack)?;

                expanded_items.push(item);
            }

            Ok(Value::of(List::new(expanded_items)))
        })
    });

    // List == Text
    env.add_relation(
        Pattern::for_trait(Trait::of::<List>()),
        Trait::of::<Text>(),
        |value, env, stack| {
            let list = value.into_primitive::<List>().unwrap();

            let mut items = Vec::new();

            for item in &list.items {
                let text = item.format(env, stack)?;
                items.push(text);
            }

            Ok(Value::of(Text::new(&format!("({})", items.join(" ")))))
        },
    );
}
