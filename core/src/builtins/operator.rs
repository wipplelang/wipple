use crate::*;
use ref_thread_local::{RefMut, RefThreadLocal};
use std::{
    collections::{HashMap, HashSet},
    hash::{Hash, Hasher},
    rc::Rc,
};

// Ensure Result inside ref_thread_local! refers to std::result::Result and not
// crate::Result
pub use operator_precedences_storage::*;
mod operator_precedences_storage {
    use super::PrecedenceGroup;
    use ref_thread_local::ref_thread_local;

    ref_thread_local! {
        pub static managed OPERATOR_PRECEDENCES: Vec<PrecedenceGroup> = Vec::new();
    }
}

pub fn operator_precedences<'a>() -> RefMut<'a, Vec<PrecedenceGroup>> {
    OPERATOR_PRECEDENCES.borrow_mut()
}

#[derive(Clone)]
pub struct SomeOperator<T> {
    pub id: Id,

    #[allow(clippy::type_complexity)]
    pub function: Rc<
        dyn Fn(T, &Environment, &Stack) -> Result<Box<dyn Fn(T, &Environment, &Stack) -> Result>>,
    >,
}

impl<T> SomeOperator<T> {
    pub fn new(
        function: impl Fn(T, &Environment, &Stack) -> Result<Box<dyn Fn(T, &Environment, &Stack) -> Result>>
            + 'static,
    ) -> Self {
        SomeOperator {
            id: Id::new(),
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
        function: impl Fn(Value, Value, &Environment, &Stack) -> Result + 'static,
    ) -> BinaryOperator {
        let function = Rc::new(function);

        BinaryOperator {
            id: Id::new(),
            function: Rc::new(move |left, _, _| {
                let function = function.clone();

                Ok(Box::new(move |right, env, stack| {
                    function(left.clone(), right, env, stack)
                }))
            }),
        }
    }

    pub fn from_function(function: Function) -> Self {
        BinaryOperator::new(move |left, env, stack| {
            let function = function(left, env, stack)?.get::<Function>(env, stack)?;

            Ok(Box::new(move |right, env, stack| {
                function(right, env, stack)
            }))
        })
    }
}

#[derive(Debug, Clone)]
pub enum VariadicOperatorInput {
    Single(Value),
    List(Vec<Value>),
}

impl VariadicOperatorInput {
    pub fn into_value(self) -> Value {
        match self {
            VariadicOperatorInput::Single(value) => value,
            VariadicOperatorInput::List(list) => Value::of(List::new(list)),
        }
    }
}

pub type VariadicOperator = SomeOperator<VariadicOperatorInput>;

impl VariadicOperator {
    pub fn collect(
        function: impl Fn(VariadicOperatorInput, VariadicOperatorInput, &Environment, &Stack) -> Result
            + 'static,
    ) -> VariadicOperator {
        let function = Rc::new(function);

        VariadicOperator {
            id: Id::new(),
            function: Rc::new(move |left, _, _| {
                let function = function.clone();

                Ok(Box::new(move |right, env, stack| {
                    function(left.clone(), right, env, stack)
                }))
            }),
        }
    }

    pub fn from_function(function: Function) -> Self {
        VariadicOperator::new(move |left, env, stack| {
            let function = function(
                match left {
                    VariadicOperatorInput::Single(value) => value,
                    VariadicOperatorInput::List(list) => Value::of(List::new(list)),
                },
                env,
                stack,
            )?
            .get::<Function>(env, stack)?;

            Ok(Box::new(move |right, env, stack| {
                function(
                    match right {
                        VariadicOperatorInput::Single(value) => value,
                        VariadicOperatorInput::List(list) => Value::of(List::new(list)),
                    },
                    env,
                    stack,
                )
            }))
        })
    }
}

#[derive(TypeInfo, Clone)]
pub enum Operator {
    Binary(BinaryOperator),
    Variadic(VariadicOperator),
}

impl Operator {
    pub fn id(&self) -> Id {
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
    pub id: Id,
    pub arity: Arity,
    pub associativity: Associativity,
    pub operators: HashSet<SomeOperator<T>>,
}

pub type BinaryPrecedenceGroup = SomePrecedenceGroup<Value>;
pub type VariadicPrecedenceGroup = SomePrecedenceGroup<VariadicOperatorInput>;

impl BinaryPrecedenceGroup {
    fn new(associativity: Associativity) -> Self {
        SomePrecedenceGroup {
            id: Id::new(),
            arity: Arity::Binary,
            associativity,
            operators: HashSet::new(),
        }
    }
}

impl VariadicPrecedenceGroup {
    fn new(associativity: Associativity) -> Self {
        SomePrecedenceGroup {
            id: Id::new(),
            arity: Arity::Variadic,
            associativity,
            operators: HashSet::new(),
        }
    }
}

#[derive(Clone)]
pub enum PrecedenceGroup {
    Binary(BinaryPrecedenceGroup),
    Variadic(VariadicPrecedenceGroup),
}

impl PrecedenceGroup {
    pub fn id(&self) -> Id {
        use PrecedenceGroup::*;

        match self {
            Binary(p) => p.id,
            Variadic(p) => p.id,
        }
    }

    pub fn arity(&self) -> Arity {
        use PrecedenceGroup::*;

        match self {
            Binary(p) => p.arity,
            Variadic(p) => p.arity,
        }
    }

    pub fn associativity(&self) -> Associativity {
        use PrecedenceGroup::*;

        match self {
            Binary(p) => p.associativity,
            Variadic(p) => p.associativity,
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
        let index = operator_precedences()
            .iter()
            .position(|o| o.id() == $target.id)
            .unwrap();

        operator_precedences().insert(index + $offset, $insert);
    };
}

macro_rules! comparisons {
    ($group:ident, $name:ident) => {
        impl PrecedenceGroupComparison<$group> {
            pub fn highest() -> Self {
                Self(Rc::new(|group| {
                    operator_precedences().insert(0, PrecedenceGroup::$name(group))
                }))
            }

            pub fn lowest() -> Self {
                Self(Rc::new(|group| {
                    operator_precedences().push(PrecedenceGroup::$name(group))
                }))
            }

            pub fn higher_than(other: &$group) -> Self {
                let other = other.clone();

                Self(Rc::new(move |group| {
                    comparison_find!(0, other, PrecedenceGroup::$name(group));
                }))
            }

            pub fn lower_than(other: &$group) -> Self {
                let other = other.clone();

                Self(Rc::new(move |group| {
                    comparison_find!(1, other, PrecedenceGroup::$name(group));
                }))
            }
        }
    };
}

comparisons!(BinaryPrecedenceGroup, Binary);
comparisons!(VariadicPrecedenceGroup, Variadic);

pub type BinaryPrecedenceGroupComparison = PrecedenceGroupComparison<BinaryPrecedenceGroup>;
pub type VariadicPrecedenceGroupComparison = PrecedenceGroupComparison<VariadicPrecedenceGroup>;

macro_rules! add_operator {
    ($group:ident, $operator:ident, $name:ident) => {
        let mut precedences = operator_precedences();

        let group = precedences
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

pub fn add_binary_precedence_group(
    associativity: Associativity,
    comparison: BinaryPrecedenceGroupComparison,
) -> BinaryPrecedenceGroup {
    let group = BinaryPrecedenceGroup::new(associativity);
    comparison.0(group.clone());
    group
}

pub fn add_variadic_precedence_group(
    associativity: Associativity,
    comparison: VariadicPrecedenceGroupComparison,
) -> VariadicPrecedenceGroup {
    let group = VariadicPrecedenceGroup::new(associativity);
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
    pub fn find_operators(&self, env: &Environment, stack: &Stack) -> Result<OperatorList> {
        let mut operators = OperatorList::new();

        for (index, item) in self.items.iter().enumerate() {
            if let Some(operator) = get_operator(item, env, stack)? {
                operators.push((operator, index));
            }
        }

        Ok(operators)
    }
}

pub fn get_operator(value: &Value, env: &Environment, stack: &Stack) -> Result<Option<Operator>> {
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
            let function = (self.function)(get_variadic_items(left), env, stack)?;

            Ok(Value::of(Function::new(move |right, env, stack| {
                function(get_variadic_items(right), env, stack)
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

fn get_variadic_items(value: Value) -> VariadicOperatorInput {
    if value.is_trait_directly(&Trait::of::<List>()) {
        let items = value.into_primitive::<List>().unwrap().items;

        if items.len() == 1 {
            VariadicOperatorInput::Single(items[0].clone())
        } else {
            VariadicOperatorInput::List(items)
        }
    } else {
        VariadicOperatorInput::Single(value)
    }
}

impl List {
    pub fn parse_operators(
        &self,
        operators_in_list: OperatorList,
        env: &Environment,
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

            let precedences = operator_precedences().clone();

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
        macro_rules! choose {
            (Left => $left:ident, Right => $right:ident) => {
                match precedence_group.associativity() {
                    Associativity::Left => sorted_operators.$left().unwrap().clone(),
                    Associativity::Right => sorted_operators.$right().unwrap().clone(),
                }
            };
        }

        let (operator, index) = match precedence_group.arity() {
            Arity::Binary => choose!(Left => first, Right => last),

            // Left and right are flipped because associativity is for inner grouping,
            // and the outer grouping is evaluated first
            Arity::Variadic => choose!(Left => last, Right => first),
        };

        match operator {
            Operator::Binary(operator) => {
                // Take a single value from each side of the operator

                let left = if index == 0 {
                    None
                } else {
                    self.items.get(index - 1).cloned()
                };

                let right = self.items.get(index + 1).cloned();

                match (left, right) {
                    (Some(left), Some(right)) => {
                        // Convert to a function call
                        let call = (operator.function)(left, env, stack)?(right, env, stack)?;

                        // Put the function call in place of the values consumed by the operator
                        let result = self.items[..(index - 1)]
                            .iter()
                            .chain(&[call])
                            .chain(&self.items[(index + 2)..])
                            .map(Clone::clone)
                            .collect::<Vec<_>>();

                        Value::of(List::new(result)).evaluate(env, stack).map(Some)
                    }
                    (Some(left), None) => {
                        // Partially apply the right side
                        let partial = Function::new(move |right, env, stack| {
                            (operator.function)(left.clone(), env, stack)?(right, env, stack)
                        });

                        let function = Value::of(partial);

                        Ok(Some(function))
                    }
                    (None, Some(right)) => {
                        // Partially apply the left side
                        let partial = Function::new(move |left, env, stack| {
                            (operator.function)(left, env, stack)?(right.clone(), env, stack)
                        });

                        let function = Value::of(partial);

                        Ok(Some(function))
                    }
                    (None, None) => unreachable!(),
                }
            }
            Operator::Variadic(operator) => {
                // Take all values from each side of the operator -- list with 2 values
                // is partially applied

                macro_rules! take {
                    ($range:expr) => {{
                        let items = self.items.get($range).unwrap_or_default();

                        if items.is_empty() {
                            None
                        } else {
                            Some(items.to_vec())
                        }
                    }};
                }

                let left = take!(..index);
                let right = take!((index + 1)..);

                fn get_variadic_input(items: Vec<Value>) -> VariadicOperatorInput {
                    if items.len() == 1 {
                        VariadicOperatorInput::Single(items[0].clone())
                    } else {
                        VariadicOperatorInput::List(items)
                    }
                }

                match (left, right) {
                    (Some(left), Some(right)) => {
                        // Convert to a function call
                        let result = (operator.function)(get_variadic_input(left), env, stack)?(
                            get_variadic_input(right),
                            env,
                            stack,
                        )?;

                        Ok(Some(result))
                    }
                    (Some(left), None) => {
                        // Partially apply the right side
                        let partial = Function::new(move |right, env, stack| {
                            (operator.function)(get_variadic_input(left.clone()), env, stack)?(
                                get_variadic_items(right),
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
                            (operator.function)(get_variadic_items(left), env, stack)?(
                                get_variadic_input(right.clone()),
                                env,
                                stack,
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

pub(crate) fn setup(env: &mut EnvironmentInner) {
    env.set_variable("Operator", Value::of(Trait::of::<Operator>()));

    // Operator == Text
    env.add_text_conformance::<Operator>("operator");
}
