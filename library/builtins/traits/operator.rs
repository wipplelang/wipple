use crate::*;
use wipple::*;

#[derive(TypeInfo, Clone, Default)]
struct OperatorPrecedences(Vec<PrecedenceGroup>);

env_key!(operator_precedences: OperatorPrecedences {
    visibility: EnvKeyVisibility::Private,
});

macro_rules! match_member {
    ($member:ident() -> $type:ty) => {
        pub fn $member(&self) -> $type {
            match self {
                Self::Binary(x) => x.$member(),
                Self::Variadic(x) => x.$member(),
            }
        }
    };
    ($member:ident: $type:ty) => {
        pub fn $member(&self) -> $type {
            match self {
                Self::Binary(x) => x.$member,
                Self::Variadic(x) => x.$member,
            }
        }
    };
}

#[derive(TypeInfo, Clone)]
pub enum Operator {
    Binary(BinaryOperator),
    Variadic(VariadicOperator),
}

impl Primitive for Operator {}

impl Operator {
    match_member!(id: Id);
}

impl From<Operator> for Function {
    fn from(operator: Operator) -> Self {
        match operator {
            Operator::Binary(operator) => operator.into(),
            Operator::Variadic(operator) => operator.into(),
        }
    }
}

pub trait OperatorTrait: Clone + From<Function> + Into<Operator> {
    type Input;

    fn new(
        apply: impl Fn(Self::Input, Self::Input, &Env, &Stack) -> Result<Value> + 'static,
    ) -> Self;

    fn id(&self) -> Id;

    fn apply(
        &self,
        left: Self::Input,
        right: Self::Input,
        env: &Env,
        stack: &Stack,
    ) -> Result<Value>;
}

macro_rules! operator {
    ($type:ident for $input:ty) => {
        paste! {
            #[derive(Clone)]
            pub struct [<$type Operator>] {
                id: Id,
                apply: Rc<dyn Fn($input, $input, &Env, &Stack) -> Result<Value>>,
            }

            impl OperatorTrait for [<$type Operator>] {
                type Input = $input;

                fn new(apply: impl Fn($input, $input, &Env, &Stack) -> Result<Value> + 'static) -> Self {
                    Self {
                        id: Id::new(),
                        apply: Rc::new(apply),
                    }
                }

                fn id(&self) -> Id {
                    self.id
                }

                fn apply(&self, left: $input, right: $input, env: &Env, stack: &Stack) -> Result<Value> {
                    (self.apply)(left, right, env, stack)
                }
            }

            impl From<[<$type Operator>]> for Operator {
                fn from(operator: [<$type Operator>]) -> Self {
                    Operator::$type(operator)
                }
            }

            impl From<Function> for [<$type Operator>] {
                fn from(f: Function) -> Self {
                    Self::new(move |left, right, env, stack| {
                        f(left.into(), env, stack)?.call_with(right.into(), env, stack)
                    })
                }
            }

            impl From<[<$type Operator>]> for Function {
                fn from(operator: [<$type Operator>]) -> Self {
                    Function::new(move |left, _, _| {
                        let operator = operator.clone();
                        let left = left.clone();

                        Ok(Value::of(Function::new(move |right, env, stack| {
                            operator.apply(left.clone().into(), right.into(), env, stack)
                        })))
                    })
                }
            }
        }
    }
}

pub enum VariadicInput {
    Single(Value),
    List(Vec<Value>),
}

impl From<Value> for VariadicInput {
    fn from(value: Value) -> Self {
        if let Some(list) = value.try_primitive().and_then(|p| p.try_cast::<List>()) {
            if list.items.len() == 1 {
                VariadicInput::Single(list.items[0].clone())
            } else {
                VariadicInput::List(list.items.clone())
            }
        } else {
            VariadicInput::Single(value)
        }
    }
}

impl From<VariadicInput> for Value {
    fn from(input: VariadicInput) -> Self {
        match input {
            VariadicInput::Single(value) => value,
            VariadicInput::List(items) => Value::of(List::new(items)),
        }
    }
}

operator!(Binary for Value);
operator!(Variadic for VariadicInput);

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

#[derive(TypeInfo, Clone)]
pub enum PrecedenceGroup {
    Binary(BinaryPrecedenceGroup),
    Variadic(VariadicPrecedenceGroup),
}

impl PrecedenceGroup {
    match_member!(id: Id);
    match_member!(arity() -> Arity);
    match_member!(associativity: Associativity);

    fn operators(&self) -> Vec<Operator> {
        match self {
            PrecedenceGroup::Binary(g) => g.operators.iter().map(|o| o.clone().into()).collect(),
            PrecedenceGroup::Variadic(g) => g.operators.iter().map(|o| o.clone().into()).collect(),
        }
    }

    fn set_operators(&mut self, operators: Vec<Operator>) {
        macro_rules! set {
            ($group:expr, $type:ident) => {
                $group.operators = operators
                    .into_iter()
                    .map(|operator| match operator {
                        Operator::$type(operator) => operator,
                        _ => unreachable!(),
                    })
                    .collect()
            };
        }

        match self {
            PrecedenceGroup::Binary(group) => set!(group, Binary),
            PrecedenceGroup::Variadic(group) => set!(group, Variadic),
        }
    }
}

pub trait PrecedenceGroupTrait: TypeInfo + Clone + Into<PrecedenceGroup> {
    type Operator: OperatorTrait;

    fn new(associativity: Associativity) -> Self;

    fn id(&self) -> Id;
    fn arity(&self) -> Arity;
    fn associativity(&self) -> Associativity;
}

macro_rules! precedence_group {
    ($type:ident) => {
        paste! {
            #[derive(TypeInfo, Clone)]
            pub struct [<$type PrecedenceGroup>] {
                id: Id,
                associativity: Associativity,
                operators: Vec<[<$type Operator>]>,
            }

            impl PrecedenceGroupTrait for [<$type PrecedenceGroup>] {
                type Operator = [<$type Operator>];

                fn new(associativity: Associativity) -> Self {
                    Self {
                        id: Id::new(),
                        associativity,
                        operators: Vec::new(),
                    }
                }

                fn id(&self) -> Id {
                    self.id
                }

                fn arity(&self) -> Arity {
                    Arity::$type
                }

                fn associativity(&self) -> Associativity {
                    self.associativity
                }
            }

            impl From<[<$type PrecedenceGroup>]> for PrecedenceGroup {
                fn from(group: [<$type PrecedenceGroup>]) -> Self {
                    PrecedenceGroup::$type(group)
                }
            }
        }
    };
}

precedence_group!(Binary);
precedence_group!(Variadic);

stored_closure!(pub struct AddPrecedenceGroupFn<P: PrecedenceGroupTrait>(P));

use std::collections::HashMap;

impl<P: PrecedenceGroupTrait> AddPrecedenceGroupFn<P> {
    pub fn highest() -> Self {
        AddPrecedenceGroupFn::new(|group: P| {
            Env::global().update_operator_precedences(|operator_precedences| {
                operator_precedences.0.insert(0, group.into());
            });
        })
    }

    pub fn lowest() -> Self {
        AddPrecedenceGroupFn::new(|group: P| {
            Env::global().update_operator_precedences(|operator_precedences| {
                operator_precedences.0.push(group.into());
            });
        })
    }

    pub fn higher_than(other: P) -> Self {
        AddPrecedenceGroupFn::new(move |group: P| {
            Env::global().update_operator_precedences(|operator_precedences| {
                let index = operator_precedences
                    .0
                    .iter()
                    .position(|g| g.id() == other.id())
                    .unwrap();

                operator_precedences.0.insert(index, group.into());
            });
        })
    }

    pub fn lower_than(other: P) -> Self {
        AddPrecedenceGroupFn::new(move |group: P| {
            Env::global().update_operator_precedences(|operator_precedences| {
                let index = operator_precedences
                    .0
                    .iter()
                    .position(|g| g.id() == other.id())
                    .unwrap();

                operator_precedences.0.insert(index + 1, group.into());
            });
        })
    }
}

pub fn add_precedence_group<P: PrecedenceGroupTrait>(
    associativity: Associativity,
    add: AddPrecedenceGroupFn<P>,
) -> P {
    let precedence_group = P::new(associativity);
    add(precedence_group.clone());
    precedence_group
}

#[macro_export]
macro_rules! add_precedence_group {
    ($associativity:expr, $($add:tt)*) => {
        $crate::add_precedence_group($associativity, AddPrecedenceGroupFn::$($add)*)
    };
}

#[ext(pub, name = AddOperatorExt)]
impl<O: OperatorTrait> O {
    fn add_to<P: PrecedenceGroupTrait<Operator = O>>(self, group: &P) {
        Env::global().update_operator_precedences(|operator_precedences| {
            let index = operator_precedences
                .0
                .iter()
                .position(|g| g.id() == group.id())
                .unwrap();

            let mut operators = operator_precedences.0[index].operators();
            operators.push(self.into());
            operator_precedences.0[index].set_operators(operators);
        });
    }
}

#[ext(pub, name = EnvAddOperatorExt)]
impl Env {
    fn add_operator<P: PrecedenceGroupTrait>(&self, name: &str, operator: P::Operator, group: &P) {
        operator.clone().add_to(group);
        self.set_constant_variable(name, Value::of(operator.into()));
    }
}

pub type OperatorList = Vec<(Operator, usize)>;

// Operator parsing

impl List {
    pub fn find_operators(&self, env: &Env, stack: &Stack) -> Result<OperatorList> {
        let mut operators = OperatorList::new();

        for (index, item) in self.items.iter().enumerate() {
            if let Some(operator) = item.get_operator(env, stack)? {
                operators.push((operator, index));
            }
        }

        Ok(operators)
    }
}

#[ext(pub, name = ValueGetOperatorExt)]
impl Value {
    fn get_operator(&self, env: &Env, stack: &Stack) -> Result<Option<Operator>> {
        macro_rules! return_operator_if_present {
            ($value:expr) => {
                if let Some(operator) = $value.get_if_present::<Operator>(env, stack)? {
                    return Ok(Some(operator.into_owned()));
                }
            };
        }

        return_operator_if_present!(self);

        if let Some(name) = self.get_if_present::<Name>(env, stack)? {
            if let Some(Variable::Constant(variable)) = name.resolve_variable_if_present(env) {
                return_operator_if_present!(variable);
            }
        }

        Ok(None)
    }
}

impl List {
    pub fn parse_operators(
        &self,
        operators_in_list: OperatorList,
        env: &Env,
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
                    Some(operator) => Ok(Some(Value::of::<Function>(operator.0.clone().into()))),
                    None => Ok(Some(self.items[0].clone())),
                }
            }
            _ => {}
        };

        let operator_precedences = Env::global().operator_precedences();

        // Sort the operators into their precedence groups
        let sorted_operators_by_precedence = {
            // FIXME: Probably very inefficient

            let mut precedences_to_sort = HashMap::new();

            for operator in operators_in_list {
                let (precedence, group) = operator_precedences
                    .0
                    .iter()
                    .enumerate()
                    .find(|p| p.1.operators().iter().any(|o| o.id() == operator.0.id()))
                    .expect("Operator is not registered");

                precedences_to_sort
                    .entry(precedence)
                    .or_insert((group.clone(), vec![operator.clone()]))
                    .1
                    .push(operator);
            }

            let mut sorted: Vec<_> = precedences_to_sort.iter().collect();
            sorted.sort_by_key(|o| o.0);
            let sorted: Vec<_> = sorted.iter().map(|o| o.1.clone()).collect();

            sorted
        };

        // Get the highest precedence group present in the list
        let (precedence_group, sorted_operators) = {
            let (group, operators) = sorted_operators_by_precedence.first().cloned().unwrap();

            let mut operators: Vec<_> = operators.to_vec();
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
                        let value = operator.apply(left, right, env, stack)?;

                        // Put the function call in place of the values consumed by the operator
                        let result = self.items[..(index - 1)]
                            .iter()
                            .chain(&[value])
                            .chain(&self.items[(index + 2)..])
                            .map(Clone::clone)
                            .collect::<Vec<_>>();

                        Ok(Some(
                            Value::of(List::new(result))
                                .evaluate(env, stack)?
                                .into_owned(),
                        ))
                    }
                    (Some(left), None) => {
                        // Partially apply the right side
                        let partial = Function::new(move |right, env, stack| {
                            operator.apply(left.clone(), right, env, stack)
                        });

                        let function = Value::of(partial);

                        Ok(Some(function))
                    }
                    (None, Some(right)) => {
                        // Partially apply the left side
                        let partial = Function::new(move |left, env, stack| {
                            operator.apply(left, right.clone(), env, stack)
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

                fn get_variadic_input(items: Vec<Value>) -> VariadicInput {
                    if items.len() == 1 {
                        VariadicInput::Single(items[0].clone())
                    } else {
                        VariadicInput::List(items)
                    }
                }

                match (left, right) {
                    (Some(left), Some(right)) => {
                        // Convert to a function call
                        let result = operator.apply(
                            get_variadic_input(left),
                            get_variadic_input(right),
                            env,
                            stack,
                        )?;

                        Ok(Some(result))
                    }
                    (Some(left), None) => {
                        // Partially apply the right side
                        let partial = Function::new(move |right, env, stack| {
                            operator.apply(
                                get_variadic_input(left.clone()),
                                right.into(),
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
                            operator.apply(
                                left.into(),
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

pub(crate) fn setup(env: &Env, stack: &Stack) -> Result<()> {
    env.set_variable("Operator", Value::of(Trait::of::<Operator>()));

    // Operator == Text
    env.add_text_relation::<Operator>("operator", stack)?;

    Ok(())
}
