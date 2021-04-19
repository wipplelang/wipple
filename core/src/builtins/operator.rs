use crate::*;
use std::{
    collections::{HashMap, HashSet},
    hash::{Hash, Hasher},
    rc::Rc,
};
use uuid::Uuid;

#[typeinfo]
#[derive(Debug, Clone, Default)]
pub struct OperatorPrecedences(pub Vec<PrecedenceGroup>);

core_env_key!(pub operator_precedences for OperatorPrecedences {
    // All operators must be declared in the global environment
    visibility: EnvironmentVisibility::Private,
});

#[typeinfo]
#[derive(Clone)]
pub struct Operator {
    pub id: Uuid,

    #[allow(clippy::type_complexity)]
    pub function: Rc<
        dyn Fn(
            &Value,
            &EnvironmentRef,
            &Stack,
        ) -> Result<Box<dyn Fn(&Value, &EnvironmentRef, &Stack) -> Result>>,
    >,
}

impl Operator {
    pub fn new(
        function: impl Fn(
                &Value,
                &EnvironmentRef,
                &Stack,
            ) -> Result<Box<dyn Fn(&Value, &EnvironmentRef, &Stack) -> Result>>
            + 'static,
    ) -> Self {
        Operator {
            id: Uuid::new_v4(),
            function: Rc::new(function),
        }
    }
}

impl PartialEq for Operator {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Operator {}

impl Hash for Operator {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl std::fmt::Debug for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Operator({:?})", self.id)
    }
}

impl Operator {
    pub fn collect(
        function: impl Fn(&Value, &Value, &EnvironmentRef, &Stack) -> Result + 'static,
    ) -> Operator {
        let function = Rc::new(function);

        Operator {
            id: Uuid::new_v4(),
            function: Rc::new(move |left, _, _| {
                let left = left.clone();
                let function = function.clone();

                Ok(Box::new(move |right, env, stack| {
                    function(&left, right, env, stack)
                }))
            }),
        }
    }

    pub fn from_function(function: Function) -> Self {
        Operator::new(move |left, env, stack| {
            let function = function(left, env, stack)?.get::<Function>(env, stack)?;

            Ok(Box::new(move |right, env, stack| {
                function(right, env, stack)
            }))
        })
    }
}

core_primitive!(pub operator for Operator);

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Arity {
    Binary,
    Variadic,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Associativity {
    Left,
    Right,
    // TODO: Add 'None' variant that prevents having multiple operators of the
    // same precedence group in the same list (useful for assignment operators)
}

pub type OperatorList = Vec<(Operator, usize)>;

#[derive(Debug, Clone)]
pub struct PrecedenceGroup {
    pub id: Uuid,
    pub operators: HashSet<Operator>,
    pub associativity: Associativity,
}

impl PrecedenceGroup {
    fn new(associativity: Associativity) -> Self {
        PrecedenceGroup {
            id: Uuid::new_v4(),
            operators: HashSet::new(),
            associativity,
        }
    }
}

impl PartialEq for PrecedenceGroup {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for PrecedenceGroup {}

impl Hash for PrecedenceGroup {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

fn_wrapper_struct! {
    pub type PrecedenceGroupComparison(PrecedenceGroup);
}

macro_rules! comparison_find {
    ($offset:expr, $target:expr, $insert:expr) => {
        let index = Environment::global()
            .borrow_mut()
            .operator_precedences()
            .0
            .iter()
            .position(|o| o.id == $target.id)
            .unwrap();

        Environment::global()
            .borrow_mut()
            .operator_precedences()
            .0
            .insert(index + $offset, $insert);
    };
}

impl PrecedenceGroupComparison {
    pub fn highest() -> Self {
        Self(Rc::new(|group| {
            Environment::global()
                .borrow_mut()
                .operator_precedences()
                .0
                .insert(0, group)
        }))
    }

    pub fn lowest() -> Self {
        Self(Rc::new(|group| {
            Environment::global()
                .borrow_mut()
                .operator_precedences()
                .0
                .push(group)
        }))
    }

    pub fn higher_than(other: PrecedenceGroup) -> Self {
        Self(Rc::new(move |group| {
            comparison_find!(0, other, group);
        }))
    }

    pub fn lower_than(other: PrecedenceGroup) -> Self {
        Self(Rc::new(move |group| {
            comparison_find!(1, other, group);
        }))
    }
}

pub fn add_precedence_group(
    associativity: Associativity,
    comparison: PrecedenceGroupComparison,
) -> PrecedenceGroup {
    let group = PrecedenceGroup::new(associativity);
    comparison.0(group.clone());
    group
}

pub fn add_operator(operator: &Operator, group: &PrecedenceGroup) {
    let env = Environment::global();
    let mut env = env.borrow_mut();

    let group = env
        .operator_precedences()
        .0
        .iter_mut()
        .find(|p| p.id == group.id)
        .expect("Invalid precedence group");

    group.operators.insert(operator.clone());
}

// Operator parsing

impl List {
    pub fn find_operators(&self, env: &EnvironmentRef, stack: &Stack) -> Result<OperatorList> {
        let mut operators = OperatorList::new();

        for (index, item) in self.items.iter().enumerate() {
            if let Some(operator) = get_operator(item, env, stack)? {
                operators.push((operator, index));
            }
        }

        Ok(operators)
    }
}

pub fn get_operator(
    value: &Value,
    env: &EnvironmentRef,
    stack: &Stack,
) -> Result<Option<Operator>> {
    match value.get_if_present::<Name>(env, stack)? {
        Some(name) => {
            let variable = name.resolve_variable_if_present(env);

            match variable {
                Some(Variable::Just(value)) => value.get_if_present::<Operator>(env, stack),
                _ => Ok(None),
            }
        }
        None => Ok(None),
    }
}

impl Operator {
    pub fn into_function(self) -> Function {
        Function::new(move |left, env, stack| {
            let function = (self.function)(left, env, stack)?;
            Ok(Value::of(Function::new(function)))
        })
    }
}

impl List {
    pub fn parse_operators(
        &self,
        operators_in_list: OperatorList,
        env: &EnvironmentRef,
        stack: &Stack,
    ) -> Result<Option<Value>> {
        // No need to parse a list containing no operators
        if operators_in_list.is_empty() {
            return Ok(None);
        }

        // List with 0/1 values isn't parsed
        match self.items.len() {
            0 => return Ok(None),
            1 => {
                return match operators_in_list.first() {
                    Some(operator) => Ok(Some(Value::of(operator.0.clone().into_function()))),
                    None => Ok(Some(self.items[0].clone())),
                }
            }
            _ => {}
        };

        // Sort the operators into their precedence groups
        let sorted_operators_by_precedence = {
            // FIXME: Probably very inefficient

            let precedences = Environment::global()
                .borrow_mut()
                .operator_precedences()
                .clone()
                .0;

            let mut precedences_to_sort = HashMap::new();

            for operator in operators_in_list {
                let (precedence, group) = precedences
                    .iter()
                    .enumerate()
                    .find(|p| p.1.operators.contains(&operator.0))
                    .expect("Operator is not registered");

                precedences_to_sort
                    .entry(precedence)
                    .or_insert((group.clone(), {
                        let mut h = HashSet::new();
                        h.insert(operator.clone());
                        h
                    }))
                    .1
                    .insert(operator);
            }

            let mut sorted: Vec<_> = precedences_to_sort.iter().collect();
            sorted.sort_by_key(|o| o.0);
            let sorted: Vec<_> = sorted.iter().map(|o| o.1.clone()).collect();

            sorted
        };

        // Get the highest precedence group present in the list
        let (precedence_group, sorted_operators) = {
            let (group, operators) = sorted_operators_by_precedence.first().cloned().unwrap();

            let mut operators: Vec<_> = operators.iter().cloned().collect();
            operators.sort_by_key(|o| o.1);

            (group, operators)
        };

        // Choose the left/rightmost operator based on the level's associativity
        // Left and right are flipped because associativity is for inner grouping,
        // and the outer grouping is evaluated first
        let (operator, index) = match precedence_group.associativity {
            Associativity::Left => sorted_operators.last().unwrap().clone(),
            Associativity::Right => sorted_operators.first().unwrap().clone(),
        };

        fn group(list: &[Value]) -> Value {
            if list.len() == 1 {
                list[0].clone()
            } else {
                Value::of(List::new(list))
            }
        }

        // Take all values from each side of the operator -- list with 2 values
        // is partially applied

        macro_rules! take {
            ($range:expr) => {{
                let items = self.items.get($range).unwrap_or_default();

                if items.is_empty() {
                    None
                } else {
                    Some(group(items))
                }
            }};
        }

        let left = take!(..index);
        let right = take!((index + 1)..);

        match (left, right) {
            (Some(left), Some(right)) => {
                // Convert to a function call
                let result = (operator.function)(&left, env, stack)?(&right, env, stack)?;

                Ok(Some(result))
            }
            (Some(left), None) => {
                // Partially apply the right side
                let partial = Function::new((operator.function)(&left, env, stack)?);

                let function = Value::of(partial);

                Ok(Some(function))
            }
            (None, Some(right)) => {
                // Partially apply the left side
                let partial = Function::new(move |left, env, stack| {
                    (operator.function)(&left, env, stack)?(&right, env, stack)
                });

                let function = Value::of(partial);

                Ok(Some(function))
            }
            (None, None) => unreachable!("Case where no values are on either side of the operator, meaning the operator is the only item in the list and it would have been returned above"),
        }
    }
}

pub(crate) fn setup(env: &mut Environment) {
    env.set_variable("Operator", Value::of(Trait::of::<Operator>()));

    // Operator == Text
    env.add_text_conformance::<Operator>("operator");
}
