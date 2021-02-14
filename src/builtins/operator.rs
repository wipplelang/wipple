use crate::builtins::*;
use crate::fundamentals::*;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use uuid::Uuid;

#[derive(Clone)]
pub struct SomeOperator<T> {
    pub id: Uuid,
    pub function:
        Rc<dyn Fn(T, &mut Environment) -> Result<Box<dyn Fn(T, &mut Environment) -> Result>>>,
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
        function: impl Fn(Value, Value, &mut Environment) -> Result + 'static,
    ) -> BinaryOperator {
        let function = Rc::new(function);

        BinaryOperator {
            id: Uuid::new_v4(),
            function: Rc::new(move |left, _| {
                let function = function.clone();

                Ok(Box::new(move |right, env| {
                    function(left.clone(), right, env).clone()
                }))
            }),
        }
    }

    pub fn from_function(function: Function) -> BinaryOperator {
        let function = Rc::new(function);

        BinaryOperator {
            id: Uuid::new_v4(),
            function: Rc::new(move |left, env| {
                let next_function = function.0(left, env)?.get_trait(TraitID::function, env)?;

                Ok(Box::new(move |right, env| next_function.0(right, env)))
            }),
        }
    }
}

pub type VariadicOperator = SomeOperator<Vec<Value>>;

impl VariadicOperator {
    pub fn collect(
        function: impl Fn(Vec<Value>, Vec<Value>, &mut Environment) -> Result + 'static,
    ) -> VariadicOperator {
        let function = Rc::new(function);

        VariadicOperator {
            id: Uuid::new_v4(),
            function: Rc::new(move |left, _| {
                let function = function.clone();

                Ok(Box::new(move |right, env| {
                    function(left.clone(), right, env)
                }))
            }),
        }
    }

    pub fn from_function(function: Function) -> VariadicOperator {
        let function = Rc::new(function);

        VariadicOperator {
            id: Uuid::new_v4(),
            function: Rc::new(move |left, env| {
                let left = Value::new(Trait::list(List(left)));
                let next_function = function.0(left, env)?.get_trait(TraitID::function, env)?;

                Ok(Box::new(move |right, env| {
                    let right = Value::new(Trait::list(List(right)));
                    next_function.0(right, env)
                }))
            }),
        }
    }
}

#[derive(Clone)]
pub enum Operator {
    Binary(BinaryOperator),
    Variadic(VariadicOperator),
}

impl Operator {
    fn id(&self) -> Uuid {
        use Operator::*;

        match self {
            Binary(o) => o.id,
            Variadic(o) => o.id,
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

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Arity {
    Binary,
    Variadic,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Associativity {
    Left,
    Right,
    // TODO: Add 'None' case that prevents having multiple operators of the same
    // precedence group (useful for assignment)
}

pub type OperatorList = Vec<(Operator, usize)>;

simple_trait! {
    name: operator,
    type: Operator,
    label: "Operator",
}

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
pub struct PrecedenceGroupComparison<P: 'static>(Rc<dyn Fn(P, &mut Environment)>);

macro_rules! comparison_find {
    ($env:ident, $offset:expr, $target:expr, $insert:expr) => {
        let index = $env
            .operator_precedences
            .iter()
            .position(|o| o.id() == $target.id)
            .unwrap();

        $env.operator_precedences.insert(index + $offset, $insert);
    };
}

macro_rules! comparisons {
    ($group:ident, $name:ident) => {
        impl PrecedenceGroupComparison<$group> {
            pub fn highest() -> Self {
                Self(Rc::new(|group, env| {
                    env.operator_precedences
                        .insert(0, PrecedenceGroup::$name(group))
                }))
            }

            pub fn lowest() -> Self {
                Self(Rc::new(|group, env| {
                    env.operator_precedences.push(PrecedenceGroup::$name(group))
                }))
            }

            pub fn higher_than(other: $group) -> Self {
                Self(Rc::new(move |group, env| {
                    comparison_find!(env, 0, other, PrecedenceGroup::$name(group));
                }))
            }

            pub fn lower_than(other: $group) -> Self {
                Self(Rc::new(move |group, env| {
                    comparison_find!(env, 1, other, PrecedenceGroup::$name(group));
                }))
            }
        }
    };
}

comparisons!(BinaryPrecedenceGroup, Binary);
comparisons!(VariadicPrecedenceGroup, Variadic);

macro_rules! add_operator {
    ($self:ident, $group:ident, $operator:ident, $name:ident) => {
        let group = $self
            .operator_precedences
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

impl Environment {
    pub fn precedence_group<P: Clone>(
        &mut self,
        associativity: Associativity,
        comparison: PrecedenceGroupComparison<SomePrecedenceGroup<P>>,
    ) -> SomePrecedenceGroup<P> {
        let group = SomePrecedenceGroup::<P>::new(associativity);

        comparison.0(group.clone(), self);

        group
    }

    pub fn add_binary_operator(
        &mut self,
        operator: &BinaryOperator,
        group: &BinaryPrecedenceGroup,
    ) {
        add_operator!(self, group, operator, Binary);
    }

    pub fn add_variadic_operator(
        &mut self,
        operator: &VariadicOperator,
        group: &VariadicPrecedenceGroup,
    ) {
        add_operator!(self, group, operator, Variadic);
    }
}

// Operator parsing

pub fn find_operators(list: &List, env: &mut Environment) -> Result<OperatorList> {
    let mut operators = OperatorList::new();

    for (index, value) in list.0.iter().enumerate() {
        if let Some(operator) = get_operator(value, env)? {
            operators.push((operator, index));
        }
    }

    Ok(operators)
}

pub fn get_operator(value: &Value, env: &mut Environment) -> Result<Option<Operator>> {
    match value.get_trait_if_present(TraitID::operator, env)? {
        Some(operator) => Ok(Some(operator)),
        None => {
            if let Some(name) = value.get_trait_if_present(TraitID::name, env)? {
                if let Some(variable) = env.variables.get(&name.0) {
                    variable
                        .clone()
                        .get_trait_if_present(TraitID::operator, env)
                } else {
                    Ok(None)
                }
            } else {
                Ok(None)
            }
        }
    }
}

impl BinaryOperator {
    pub fn as_function(self) -> Function {
        Function::new(move |left, env| {
            let function = (self.function)(left, env)?;
            Ok(Value::new(Trait::function(Function::new(function))))
        })
    }
}

impl VariadicOperator {
    pub fn as_function(self) -> Function {
        Function::new(move |left, env| {
            let function = (self.function)(get_variadic_items(left, env)?, env)?;

            Ok(Value::new(Trait::function(Function::new(
                move |right, env| function(get_variadic_items(right, env)?, env),
            ))))
        })
    }
}

impl Operator {
    pub fn as_function(self) -> Function {
        use Operator::*;

        match self {
            Binary(o) => o.as_function(),
            Variadic(o) => o.as_function(),
        }
    }
}

pub fn get_variadic_items(list: Value, env: &mut Environment) -> Result<Vec<Value>> {
    match list.get_trait_if_present(TraitID::list, env)? {
        Some(list) => Ok(list.0),
        None => Err(ProgramError::new(
            "Application of variadic operator requires a list",
        )),
    }
}

pub fn parse_operators(
    list: List,
    operators_in_list: OperatorList,
    env: &mut Environment,
) -> Result<Option<Value>> {
    // No need to parse a list containing no operators
    if operators_in_list.is_empty() {
        return Ok(None);
    }

    // List with 0/1 values isn't parsed
    match list.0.len() {
        0 => return Ok(None),
        1 => {
            return match operators_in_list.first() {
                Some(operator) => Ok(Some(Value::new(Trait::function(
                    operator.0.clone().as_function(),
                )))),
                None => Ok(Some(list.0[0].clone())),
            }
        }
        _ => {}
    };

    // Sort the operators into their precedence groups
    let sorted_operators_by_precedence = {
        // TODO: Probably very inefficient

        let mut precedences = HashMap::new();

        for operator in operators_in_list {
            let (precedence, group) = env
                .operator_precedences
                .iter()
                .enumerate()
                .find(|p| p.1.operators().contains(&operator.0))
                .expect("Operator is not registered");

            precedences
                .entry(precedence)
                .or_insert((group.clone(), {
                    let mut h = HashSet::new();
                    h.insert(operator.clone());
                    h
                }))
                .1
                .insert(operator);
        }

        let mut sorted: Vec<_> = precedences.iter().collect();
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
    let (operator, index) = match precedence_group.associativity() {
        Associativity::Left => sorted_operators.first().unwrap().clone(),
        Associativity::Right => sorted_operators.last().unwrap().clone(),
    };

    // List with 2 values is partially applied
    match operator {
        Operator::Binary(operator) => {
            // Take a single value from each side of the operator
            let left = list.0.get(index - 1).cloned();
            let right = list.0.get(index + 1).cloned();

            match (left, right) {
                (Some(left), Some(right)) => {
                    // Convert to a function call
                    let result = (operator.function)(left.clone(), env)?(right.clone(), env)?;

                    Ok(Some(result))
                }
                (Some(left), None) => {
                    // Partially apply the right side
                    let partial = Function::new(move |right, env| {
                        (operator.function)(left.clone(), env)?(right, env)
                    });

                    let function = Value::new(Trait::function(partial));

                    Ok(Some(function))
                }
                (None, Some(right)) => {
                    // Partially apply the left side
                    let partial = Function::new(move |left, env| {
                        (operator.function)(left, env)?(right.clone(), env)
                    });

                    let function = Value::new(Trait::function(partial));

                    Ok(Some(function))
                }
                (None, None) => unreachable!(),
            }
        }
        Operator::Variadic(operator) => {
            // Take all values from each side of the operator
            let left = list.0.get(..index).map(|v| v.to_vec());
            let right = list.0.get(index..).map(|v| v.to_vec());

            match (left, right) {
                (Some(left), Some(right)) => {
                    // Convert to a function call
                    let result = (operator.function)(left, env)?(right, env)?;

                    Ok(Some(result))
                }
                (Some(left), None) => {
                    // Partially apply the right side
                    let partial = Function::new(move |right, env| {
                        (operator.function)(left.clone(), env)?(
                            get_variadic_items(right, env)?,
                            env,
                        )
                    });

                    let function = Value::new(Trait::function(partial));

                    Ok(Some(function))
                }
                (None, Some(right)) => {
                    // Partially apply the left side
                    let partial = Function::new(move |left, env| {
                        (operator.function)(get_variadic_items(left, env)?, env)?(
                            right.clone(),
                            env,
                        )
                    });

                    let function = Value::new(Trait::function(partial));

                    Ok(Some(function))
                }
                (None, None) => unreachable!(),
            }
        }
    }
}
