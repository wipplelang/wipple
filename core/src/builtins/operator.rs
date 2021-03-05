use crate::*;
use std::{
    collections::{HashMap, HashSet},
    hash::{Hash, Hasher},
    rc::Rc,
};
use uuid::Uuid;

pub type OperatorPrecedences = Vec<PrecedenceGroup>;

env_key!(operator_precedences for OperatorPrecedences {
    EnvironmentKey::new(
        UseFn::take_parent::<OperatorPrecedences>(),
        true,
    )
});

#[derive(Clone)]
pub struct SomeOperator<T> {
    pub id: Uuid,

    #[allow(clippy::type_complexity)]
    pub function: Rc<
        dyn Fn(
            &T,
            &EnvironmentRef,
            &Stack,
        ) -> Result<Box<dyn Fn(&T, &EnvironmentRef, &Stack) -> Result>>,
    >,
}

impl<T> SomeOperator<T> {
    pub fn new(
        function: impl Fn(
                &T,
                &EnvironmentRef,
                &Stack,
            ) -> Result<Box<dyn Fn(&T, &EnvironmentRef, &Stack) -> Result>>
            + 'static,
    ) -> Self {
        SomeOperator {
            id: Uuid::new_v4(),
            function: Rc::new(function),
        }
    }
}

impl<T> PartialEq for SomeOperator<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<T> Eq for SomeOperator<T> {}

impl<T> Hash for SomeOperator<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

pub type BinaryOperator = SomeOperator<Value>;

impl BinaryOperator {
    pub fn collect(
        function: impl Fn(&Value, &Value, &EnvironmentRef, &Stack) -> Result + 'static,
    ) -> BinaryOperator {
        let function = Rc::new(function);

        BinaryOperator {
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
        BinaryOperator::new(move |left, env, stack| {
            let function = function.0(left, env, stack)?.get_primitive::<Function>(env, stack)?;

            Ok(Box::new(move |right, env, stack| {
                function.0(right, env, stack)
            }))
        })
    }
}

pub type VariadicOperator = SomeOperator<Vec<Value>>;

impl VariadicOperator {
    pub fn collect(
        function: impl Fn(&Vec<Value>, &Vec<Value>, &EnvironmentRef, &Stack) -> Result + 'static,
    ) -> VariadicOperator {
        let function = Rc::new(function);

        VariadicOperator {
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
        VariadicOperator::new(move |left, env, stack| {
            let function = function.0(
                &Value::of(List {
                    items: left.clone(),
                    location: None,
                }),
                env,
                stack,
            )?
            .get_primitive::<Function>(env, stack)?;

            Ok(Box::new(move |right, env, stack| {
                function.0(
                    &Value::of(List {
                        items: right.clone(),
                        location: None,
                    }),
                    env,
                    stack,
                )
            }))
        })
    }
}

#[derive(Clone)]
pub enum Operator {
    Binary(BinaryOperator),
    Variadic(VariadicOperator),
}

impl Operator {
    pub fn id(&self) -> Uuid {
        match self {
            Operator::Binary(operator) => operator.id,
            Operator::Variadic(operator) => operator.id,
        }
    }
}

impl PartialEq for Operator {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

impl Eq for Operator {}

impl Hash for Operator {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id().hash(state);
    }
}

primitive!(operator for Operator);

pub(crate) fn setup(env: &mut Environment) {
    // Operator ::= Text
    env.add_conformance_for_primitive(TraitID::text(), |_: Operator, _, _| {
        Ok(Some(Value::of(Text {
            text: String::from("<operator>"),
            location: None,
        })))
    });
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Arity {
    Binary,
    Variadic,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Associativity {
    Left,
    Right,
    // TODO: Add 'None' variant that prevents having multiple operators of the
    // same precedence group in the same list (useful for assignment operators)
}

pub type OperatorList = Vec<(Operator, usize)>;

#[derive(Clone)]
pub struct SomePrecedenceGroup<T> {
    pub id: Uuid,
    pub operators: HashSet<SomeOperator<T>>,
    pub associativity: Associativity,
}

pub type BinaryPrecedenceGroup = SomePrecedenceGroup<Value>;
pub type VariadicPrecedenceGroup = SomePrecedenceGroup<Vec<Value>>;

impl<O> SomePrecedenceGroup<O> {
    fn new(associativity: Associativity) -> SomePrecedenceGroup<O> {
        SomePrecedenceGroup {
            id: Uuid::new_v4(),
            operators: HashSet::new(),
            associativity,
        }
    }
}

#[derive(Clone)]
pub enum PrecedenceGroup {
    Binary(BinaryPrecedenceGroup),
    Variadic(VariadicPrecedenceGroup),
}

impl PrecedenceGroup {
    pub fn id(&self) -> Uuid {
        use PrecedenceGroup::*;

        match self {
            Binary(p) => p.id,
            Variadic(p) => p.id,
        }
    }

    pub fn operators(&self) -> HashSet<Operator> {
        use PrecedenceGroup::*;

        match self {
            Binary(p) => p
                .operators
                .iter()
                .map(|o| Operator::Binary(o.clone()))
                .collect(),
            Variadic(p) => p
                .operators
                .iter()
                .map(|o| Operator::Variadic(o.clone()))
                .collect(),
        }
    }

    pub fn associativity(&self) -> Associativity {
        use PrecedenceGroup::*;

        match self {
            Binary(p) => p.associativity,
            Variadic(p) => p.associativity,
        }
    }
}

impl PartialEq for PrecedenceGroup {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

impl Eq for PrecedenceGroup {}

impl Hash for PrecedenceGroup {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id().hash(state);
    }
}

#[derive(Clone)]
pub struct PrecedenceGroupComparison<P: 'static>(Rc<dyn Fn(P)>);

macro_rules! comparison_find {
    ($offset:expr, $target:expr, $insert:expr) => {
        let index = Environment::global()
            .borrow_mut()
            .operator_precedences()
            .iter()
            .position(|o| o.id() == $target.id)
            .unwrap();

        Environment::global()
            .borrow_mut()
            .operator_precedences()
            .insert(index + $offset, $insert);
    };
}

macro_rules! comparisons {
    ($group:ident, $name:ident) => {
        impl PrecedenceGroupComparison<$group> {
            pub fn highest() -> Self {
                Self(Rc::new(|group| {
                    Environment::global()
                        .borrow_mut()
                        .operator_precedences()
                        .insert(0, PrecedenceGroup::$name(group))
                }))
            }

            pub fn lowest() -> Self {
                Self(Rc::new(|group| {
                    Environment::global()
                        .borrow_mut()
                        .operator_precedences()
                        .push(PrecedenceGroup::$name(group))
                }))
            }

            pub fn higher_than(other: $group) -> Self {
                Self(Rc::new(move |group| {
                    comparison_find!(0, other, PrecedenceGroup::$name(group));
                }))
            }

            pub fn lower_than(other: $group) -> Self {
                Self(Rc::new(move |group| {
                    comparison_find!(1, other, PrecedenceGroup::$name(group));
                }))
            }
        }
    };
}

comparisons!(BinaryPrecedenceGroup, Binary);
comparisons!(VariadicPrecedenceGroup, Variadic);

macro_rules! add_operator {
    ($group:ident, $operator:ident, $name:ident) => {
        let env = Environment::global();
        let mut env = env.borrow_mut();

        let group = env
            .operator_precedences()
            .iter_mut()
            .find_map(|p| match p {
                PrecedenceGroup::$name(p) => {
                    if p.id == $group.id {
                        Some(p)
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .expect("Invalid precedence group; make sure to create all precedence groups using Environment::precedence_group");

        group.operators.insert($operator.clone());
    };
}

pub fn add_precedence_group<P: Clone>(
    associativity: Associativity,
    comparison: PrecedenceGroupComparison<SomePrecedenceGroup<P>>,
) -> SomePrecedenceGroup<P> {
    let group = SomePrecedenceGroup::<P>::new(associativity);

    comparison.0(group.clone());

    group
}

pub fn add_binary_operator(operator: &BinaryOperator, group: &BinaryPrecedenceGroup) {
    add_operator!(group, operator, Binary);
}

pub fn add_variadic_operator(operator: &VariadicOperator, group: &VariadicPrecedenceGroup) {
    add_operator!(group, operator, Variadic);
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
    if let Some(name) = value.get_primitive_if_present::<Name>(env, stack)? {
        let variable = name.resolve_if_present(env);

        if let Some(variable) = variable {
            variable.get_primitive_if_present::<Operator>(env, stack)
        } else {
            Ok(None)
        }
    } else {
        Ok(None)
    }
}

impl BinaryOperator {
    pub fn into_function(self) -> Function {
        Function::new(move |left, env, stack| {
            let function = (self.function)(left, env, stack)?;
            Ok(Value::of(Function::new(function)))
        })
    }
}

impl VariadicOperator {
    pub fn into_function(self) -> Function {
        Function::new(move |left, env, stack| {
            let function = (self.function)(&get_variadic_items(left, env, stack)?, env, stack)?;

            Ok(Value::of(Function::new(move |right, env, stack| {
                function(&get_variadic_items(right, env, stack)?, env, stack)
            })))
        })
    }
}

impl Operator {
    pub fn into_function(self) -> Function {
        use Operator::*;

        match self {
            Binary(o) => o.into_function(),
            Variadic(o) => o.into_function(),
        }
    }
}

fn get_variadic_items(value: &Value, env: &EnvironmentRef, stack: &Stack) -> Result<Vec<Value>> {
    value
        .get_primitive_or::<List>(
            "Application of variadic operator requires a list",
            env,
            stack,
        )
        .map(|list| list.items)
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
                .clone();

            let mut precedences_to_sort = HashMap::new();

            for operator in operators_in_list {
                let (precedence, group) = precedences
                    .iter()
                    .enumerate()
                    .find(|p| p.1.operators().contains(&operator.0))
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
        let (operator, index) = match precedence_group.associativity() {
            Associativity::Left => sorted_operators.last().unwrap().clone(),
            Associativity::Right => sorted_operators.first().unwrap().clone(),
        };

        // List with 2 values is partially applied
        match operator {
            Operator::Binary(operator) => {
                // Take a single value from each side of the operator
                let left = self.items.get(index - 1).cloned();
                let right = self.items.get(index + 1).cloned();

                match (left, right) {
                    (Some(left), Some(right)) => {
                        // Convert to a function call
                        let result = (operator.function)(&left, env, stack)?(&right, env, stack)?;

                        Ok(Some(result))
                    }
                    (Some(left), None) => {
                        // Partially apply the right side
                        let partial = Function::new(move |right, env, stack| {
                            (operator.function)(&left, env, stack)?(right, env, stack)
                        });

                        let function = Value::of(partial);

                        Ok(Some(function))
                    }
                    (None, Some(right)) => {
                        // Partially apply the left side
                        let partial = Function::new(move |left, env, stack| {
                            (operator.function)(left, env, stack)?(&right, env, stack)
                        });

                        let function = Value::of(partial);

                        Ok(Some(function))
                    }
                    (None, None) => unreachable!(),
                }
            }
            Operator::Variadic(operator) => {
                // Take all values from each side of the operator
                let left = self.items.get(..index).map(|v| v.to_vec());
                let right = self.items.get((index + 1)..).map(|v| v.to_vec());

                match (left, right) {
                    (Some(left), Some(right)) => {
                        // Convert to a function call
                        let result = (operator.function)(&left, env, stack)?(&right, env, stack)?;

                        Ok(Some(result))
                    }
                    (Some(left), None) => {
                        // Partially apply the right side
                        let partial = Function::new(move |right, env, stack| {
                            (operator.function)(&left, env, stack)?(
                                &get_variadic_items(right, env, stack)?,
                                env,
                                stack,
                            )
                        });

                        let function = Value::of(partial);

                        Ok(Some(function))
                    }
                    (None, Some(right)) => {
                        // Partially apply the left side
                        let partial = Function::new(move |left, env, stack| {
                            (operator.function)(&get_variadic_items(left, env, stack)?, env, stack)?(
                                &right, env, stack,
                            )
                        });

                        let function = Value::of(partial);

                        Ok(Some(function))
                    }
                    (None, None) => unreachable!(),
                }
            }
        }
    }
}
