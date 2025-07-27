mod js;

use crate::{
    lower::Path,
    typecheck::{self, Driver},
    util::WithInfo,
};
use serde::Serialize;
use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::Display,
    mem,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id(u32);

impl Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "_{}", self.0)
    }
}

fn serialize_id<S: serde::Serializer>(id: &Id, serializer: S) -> Result<S::Ok, S::Error> {
    serializer.serialize_str(&id.to_string())
}

#[derive(Debug, Clone)]
pub struct Executable {
    pub items: BTreeMap<Id, (Path, WithInfo<Vec<Statement>>)>,
    pub instances: BTreeMap<Id, Vec<Instance>>,
    pub entrypoint: Id,
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub id: Id,
    pub substitutions: BTreeMap<Id, Type>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    If {
        conditions: Vec<Condition>,
        statements: Vec<Statement>,
        is_else: bool,
    },
    Expression(WithInfo<Expression>),
    Return(WithInfo<Expression>),
}

#[derive(Debug, Clone)]
pub enum Condition {
    Assign(Id, WithInfo<Expression>),
    Element(Id, Id, u32),
    IsVariant(Id, u32),
    IsNumber(Id, String),
    IsText(Id, String),
    Or(Vec<Condition>, Vec<Condition>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Variable(Id),
    Constant(Id, BTreeMap<Id, Type>),
    Trait(Id, BTreeMap<Id, Type>),
    Call(Box<WithInfo<Expression>>, Vec<WithInfo<Expression>>),
    Marker,
    List(Vec<WithInfo<Expression>>),
    Variant(u32, Vec<WithInfo<Expression>>),
    Intrinsic(String, Vec<WithInfo<Expression>>),
    Text(String),
    Number(String),
    Format(Vec<(String, WithInfo<Expression>)>, String),
    Function(Vec<Id>, BTreeSet<Id>, Vec<Statement>),
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum Type {
    Parameter(#[serde(serialize_with = "serialize_id")] Id),
    Named(#[serde(serialize_with = "serialize_id")] Id, Vec<Type>),
    Function(Vec<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Block(Box<Type>),
    Intrinsic,
    Equal(Box<Type>, Box<Type>),
}

pub struct Codegen<'a> {
    driver: &'a dyn Driver,
    next_id: Id,
    ids: HashMap<Path, Id>,
    vars: BTreeSet<Id>,
    items: BTreeMap<Id, (Path, WithInfo<Vec<Statement>>)>,
    instances: BTreeMap<Id, Vec<Instance>>,
    default_instances: BTreeMap<Id, Vec<Instance>>,
}

impl<'a> Codegen<'a> {
    pub fn new(driver: &'a dyn Driver) -> Self {
        Codegen {
            driver,
            next_id: Id(0),
            ids: Default::default(),
            vars: Default::default(),
            items: Default::default(),
            instances: Default::default(),
            default_instances: Default::default(),
        }
    }

    pub fn into_executable(self, entrypoint: &Path) -> Executable {
        let entrypoint = *self.ids.get(entrypoint).unwrap();

        // Place default instances after non-default instances
        let mut instances = self.instances.clone();
        for (r#trait, default_instances) in &self.default_instances {
            instances
                .entry(*r#trait)
                .or_default()
                .extend(default_instances.iter().cloned());
        }

        Executable {
            items: self.items,
            instances,
            entrypoint,
        }
    }

    pub fn insert_instance(
        &mut self,
        path: &Path,
        instance: WithInfo<&typecheck::InstanceDeclaration>,
    ) -> Result<(), ()> {
        let id = self.path(path);

        let trait_id = self.path(&instance.item.instance.item.r#trait);

        let trait_parameters = self
            .driver
            .get_trait_declaration(&instance.item.instance.item.r#trait)
            .item
            .parameters
            .into_iter()
            .map(|path| self.path(&path))
            .collect::<Vec<_>>();

        let instance_parameters = instance
            .item
            .instance
            .item
            .parameters
            .iter()
            .map(|ty| self.ty(&ty.item))
            .collect::<Result<Vec<_>, _>>()?;

        let substitutions = trait_parameters
            .into_iter()
            .zip(instance_parameters)
            .collect();

        let instances = if instance.item.default {
            &mut self.default_instances
        } else {
            &mut self.instances
        };

        instances
            .entry(trait_id)
            .or_default()
            .push(Instance { id, substitutions });

        Ok(())
    }

    pub fn insert_item(
        &mut self,
        path: &Path,
        body: WithInfo<&typecheck::TypedExpression>,
    ) -> Result<Id, ()> {
        let id = self.path(path);

        let mut statements = Vec::new();
        let result = self
            .expression(body.as_deref(), &mut statements)?
            .unwrap_or_else(|| body.replace(Expression::Marker));
        statements.push(Statement::Return(result));

        self.items
            .insert(id, (path.clone(), body.replace(statements)));

        Ok(id)
    }

    fn expression(
        &mut self,
        expression: WithInfo<&typecheck::TypedExpression>,
        statements: &mut Vec<Statement>,
    ) -> Result<Option<WithInfo<Expression>>, ()> {
        let result = match &expression.item.kind {
            typecheck::TypedExpressionKind::Unknown(_) => return Err(()),
            typecheck::TypedExpressionKind::Variable(_, path) => {
                let id = self.path(path);
                Some(Expression::Variable(id))
            }
            typecheck::TypedExpressionKind::Constant {
                path,
                substitutions: parameters,
                ..
            } => {
                let id = self.path(path);

                let substitutions = parameters
                    .iter()
                    .map(|(parameter, ty)| Ok((self.path(parameter), self.ty(ty)?)))
                    .collect::<Result<_, _>>()?;

                Some(Expression::Constant(id, substitutions))
            }
            typecheck::TypedExpressionKind::Trait {
                path,
                substitutions,
                ..
            } => {
                let id = self.path(path);

                let substitutions = substitutions
                    .iter()
                    .map(|(parameter, ty)| Ok((self.path(parameter), self.ty(ty)?)))
                    .collect::<Result<_, _>>()?;

                Some(Expression::Trait(id, substitutions))
            }
            typecheck::TypedExpressionKind::Number(value) => {
                Some(Expression::Number(value.clone()))
            }
            typecheck::TypedExpressionKind::Text(value) => Some(Expression::Text(value.clone())),
            typecheck::TypedExpressionKind::Block {
                statements: body, ..
            } => {
                let prev_vars = mem::take(&mut self.vars);

                let mut statements = Vec::new();
                let mut has_value = false;
                for (index, statement) in body.iter().enumerate() {
                    if let Some(expression) =
                        self.expression(statement.as_ref(), &mut statements)?
                    {
                        has_value = true;

                        if index + 1 == body.len() {
                            statements.push(Statement::Return(expression));
                        } else {
                            statements.push(Statement::Expression(expression));
                        }
                    }
                }

                if !has_value {
                    statements.push(Statement::Return(expression.replace(Expression::Marker)));
                }

                let vars = mem::replace(&mut self.vars, prev_vars);

                Some(Expression::Function(Vec::new(), vars, statements))
            }
            typecheck::TypedExpressionKind::Do(input) => {
                let input = self.expression(input.as_deref(), statements)?;
                Some(Expression::Call(Box::new(input.ok_or(())?), Vec::new()))
            }
            typecheck::TypedExpressionKind::Function { inputs, body, .. } => {
                let prev_vars = mem::take(&mut self.vars);

                let mut conditions = Vec::new();
                let inputs = inputs
                    .iter()
                    .map(|input| {
                        let id = self.id();
                        self.pattern(id, input.as_ref(), &mut conditions)?;
                        Ok(id)
                    })
                    .collect::<Result<Vec<_>, ()>>()?;

                let mut statements = Vec::new();
                let result = self
                    .expression(body.as_deref(), &mut statements)?
                    .ok_or(())?;
                statements.push(Statement::Return(result));

                let vars = mem::replace(&mut self.vars, prev_vars);

                Some(Expression::Function(
                    inputs,
                    vars,
                    vec![Statement::If {
                        conditions,
                        statements,
                        is_else: false,
                    }],
                ))
            }
            typecheck::TypedExpressionKind::Call { function, inputs } => {
                let function = self
                    .expression(function.as_deref(), statements)?
                    .ok_or(())?;

                let inputs = inputs
                    .iter()
                    .map(|input| self.expression(input.as_ref(), statements)?.ok_or(()))
                    .collect::<Result<Vec<_>, _>>()?;

                Some(Expression::Call(Box::new(function), inputs))
            }
            typecheck::TypedExpressionKind::When { input, arms } => {
                let input = self.expression(input.as_deref(), statements)?.ok_or(())?;

                let input_id = self.var();
                statements.push(Statement::If {
                    conditions: vec![Condition::Assign(input_id, input)],
                    statements: Vec::new(),
                    is_else: false,
                });

                let output_id = self.var();
                for (index, arm) in arms.iter().enumerate() {
                    let mut arm_conditions = Vec::new();
                    self.pattern(input_id, arm.item.pattern.as_ref(), &mut arm_conditions)?;

                    let mut arm_statements = Vec::new();
                    let result = self
                        .expression(arm.item.body.as_ref(), &mut arm_statements)?
                        .ok_or(())?;

                    arm_statements.push(Statement::If {
                        conditions: vec![Condition::Assign(output_id, result)],
                        statements: Vec::new(),
                        is_else: false,
                    });

                    statements.push(Statement::If {
                        conditions: arm_conditions,
                        statements: arm_statements,
                        is_else: index > 0,
                    });
                }

                Some(Expression::Variable(output_id))
            }
            typecheck::TypedExpressionKind::Intrinsic { name, inputs } => {
                let inputs = inputs
                    .iter()
                    .map(|input| self.expression(input.as_ref(), statements)?.ok_or(()))
                    .collect::<Result<Vec<_>, _>>()?;

                Some(Expression::Intrinsic(name.clone(), inputs))
            }
            typecheck::TypedExpressionKind::Initialize { pattern, value } => {
                let value = self.expression(value.as_deref(), statements)?.ok_or(())?;

                let value_id = self.var();
                statements.push(Statement::If {
                    conditions: vec![Condition::Assign(value_id, value)],
                    statements: Vec::new(),
                    is_else: false,
                });

                let mut conditions = Vec::new();
                self.pattern(value_id, pattern.as_ref(), &mut conditions)?;

                statements.push(Statement::If {
                    conditions,
                    statements: Vec::new(),
                    is_else: false,
                });

                None
            }
            typecheck::TypedExpressionKind::Mutate { path, value, .. } => {
                let value = self.expression(value.as_deref(), statements)?.ok_or(())?;

                let id = self.path(&path.item);
                statements.push(Statement::If {
                    conditions: vec![Condition::Assign(id, value)],
                    statements: Vec::new(),
                    is_else: false,
                });

                None
            }
            typecheck::TypedExpressionKind::Marker(_) => Some(Expression::Marker),
            typecheck::TypedExpressionKind::Structure { fields, .. } => {
                let mut fields = fields
                    .iter()
                    .map(|field| {
                        let index = field.item.index.ok_or(())?;

                        let value = self
                            .expression(field.item.value.as_ref(), statements)?
                            .ok_or(())?;

                        Ok((index, value))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                fields.sort_by_key(|(index, _)| *index);

                let fields = fields
                    .into_iter()
                    .map(|(_, value)| value)
                    .collect::<Vec<_>>();

                Some(Expression::List(fields))
            }
            typecheck::TypedExpressionKind::Variant { variant, values } => {
                let enumeration_path = self.driver.get_enumeration_for_variant(&variant.item);
                let enumeration = self.driver.get_type_declaration(&enumeration_path);

                let variant_index = match enumeration.item.representation.item {
                    crate::typecheck::TypeRepresentation::Enumeration(variants) => variants
                        .iter()
                        .find_map(|(path, value)| {
                            (path == &variant.item).then_some(value.item.index)
                        })
                        .ok_or(())?,
                    _ => return Err(()),
                };

                let values = values
                    .iter()
                    .map(|value| self.expression(value.as_ref(), statements)?.ok_or(()))
                    .collect::<Result<Vec<_>, _>>()?;

                Some(Expression::Variant(variant_index, values))
            }
            typecheck::TypedExpressionKind::Wrapper(value) => {
                let value = self.expression(value.as_deref(), statements)?.ok_or(())?;
                Some(Expression::List(vec![value]))
            }
            typecheck::TypedExpressionKind::Tuple(items) => {
                let items = items
                    .iter()
                    .map(|item| self.expression(item.as_ref(), statements)?.ok_or(()))
                    .collect::<Result<Vec<_>, _>>()?;

                Some(Expression::List(items))
            }
            typecheck::TypedExpressionKind::Format { segments, trailing } => {
                let segments = segments
                    .iter()
                    .map(|segment| {
                        let expression = self
                            .expression(segment.value.as_ref(), statements)?
                            .ok_or(())?;

                        Ok((segment.text.clone(), expression))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                Some(Expression::Format(segments, trailing.clone()))
            }
        };

        Ok(result.map(|result| expression.replace(result)))
    }

    fn pattern(
        &mut self,
        input: Id,
        pattern: WithInfo<&typecheck::Pattern>,
        conditions: &mut Vec<Condition>,
    ) -> Result<(), ()> {
        match &pattern.item {
            typecheck::Pattern::Unknown => Err(()),
            typecheck::Pattern::Wildcard => Ok(()),
            typecheck::Pattern::Number(value) => {
                conditions.push(Condition::IsNumber(input, value.clone()));
                Ok(())
            }
            typecheck::Pattern::Text(value) => {
                conditions.push(Condition::IsText(input, value.clone()));
                Ok(())
            }
            typecheck::Pattern::Variable(_, path) => {
                let id = self.path(path);
                self.vars.insert(id);
                conditions.push(Condition::Assign(
                    id,
                    pattern.replace(Expression::Variable(input)),
                ));
                Ok(())
            }
            typecheck::Pattern::Destructure {
                structure,
                field_patterns,
            } => {
                let structure = &structure.as_ref().ok_or(())?.item;

                let field_indices = match self
                    .driver
                    .get_type_declaration(structure)
                    .item
                    .representation
                    .item
                {
                    crate::typecheck::TypeRepresentation::Structure(fields) => fields
                        .into_iter()
                        .map(|(name, field)| (name, field.item.index))
                        .collect::<HashMap<_, _>>(),
                    _ => return Err(()),
                };

                for field in field_patterns {
                    let field_index = field_indices.get(&field.item.name).copied().ok_or(())?;

                    let field_input = self.var();
                    conditions.push(Condition::Element(field_input, input, field_index));
                    self.pattern(field_input, field.item.pattern.as_ref(), conditions)?;
                }

                Ok(())
            }
            typecheck::Pattern::Variant {
                variant,
                value_patterns,
            } => {
                let enumeration = self.driver.get_enumeration_for_variant(&variant.item);

                let variant_index = match self
                    .driver
                    .get_type_declaration(&enumeration)
                    .item
                    .representation
                    .item
                {
                    crate::typecheck::TypeRepresentation::Enumeration(variants) => variants
                        .into_iter()
                        .find_map(|(path, value)| {
                            (path == variant.item).then_some(value.item.index)
                        })
                        .ok_or(())?,
                    _ => return Err(()),
                };

                conditions.push(Condition::IsVariant(input, variant_index));

                for (index, value_pattern) in value_patterns.iter().enumerate() {
                    let value_input = self.var();
                    conditions.push(Condition::Element(value_input, input, index as u32));
                    self.pattern(value_input, value_pattern.as_ref(), conditions)?;
                }

                Ok(())
            }
            typecheck::Pattern::Marker(_) => Ok(()),
            typecheck::Pattern::Wrapper { value_pattern, .. } => {
                let value_input = self.var();
                conditions.push(Condition::Element(value_input, input, 0));
                self.pattern(value_input, value_pattern.as_deref(), conditions)?;
                Ok(())
            }
            typecheck::Pattern::Tuple(elements) => {
                for (index, element) in elements.iter().enumerate() {
                    let element_input = self.var();
                    conditions.push(Condition::Element(element_input, input, index as u32));
                    self.pattern(element_input, element.as_ref(), conditions)?;
                }

                Ok(())
            }
            typecheck::Pattern::Or { left, right } => {
                let mut left_conditions = Vec::new();
                self.pattern(input, left.as_deref(), &mut left_conditions)?;

                let mut right_conditions = Vec::new();
                self.pattern(input, right.as_deref(), &mut right_conditions)?;

                conditions.push(Condition::Or(left_conditions, right_conditions));
                Ok(())
            }
            typecheck::Pattern::Annotate { pattern, .. } => {
                self.pattern(input, pattern.as_deref(), conditions)
            }
        }
    }

    fn ty(&mut self, ty: &typecheck::Type) -> Result<Type, ()> {
        match &ty {
            typecheck::Type::Unknown => Err(()),
            typecheck::Type::Parameter(path) => Ok(Type::Parameter(self.path(path))),
            typecheck::Type::Declared { path, parameters } => Ok(Type::Named(
                self.path(path),
                parameters
                    .iter()
                    .map(|ty| self.ty(&ty.item))
                    .collect::<Result<_, _>>()?,
            )),
            typecheck::Type::Function { inputs, output } => Ok(Type::Function(
                inputs
                    .iter()
                    .map(|ty| self.ty(&ty.item))
                    .collect::<Result<_, _>>()?,
                Box::new(self.ty(&output.item)?),
            )),
            typecheck::Type::Tuple(items) => Ok(Type::Tuple(
                items
                    .iter()
                    .map(|ty| self.ty(&ty.item))
                    .collect::<Result<_, _>>()?,
            )),
            typecheck::Type::Block(output) => Ok(Type::Block(Box::new(self.ty(&output.item)?))),
            typecheck::Type::Intrinsic | typecheck::Type::Message { .. } => Ok(Type::Intrinsic),
            typecheck::Type::Equal { left, right } => Ok(Type::Equal(
                Box::new(self.ty(&left.item)?),
                Box::new(self.ty(&right.item)?),
            )),
        }
    }

    fn path(&mut self, path: &Path) -> Id {
        if let Some(id) = self.ids.get(path) {
            return *id;
        }

        let id = self.id();
        self.ids.insert(path.clone(), id);
        id
    }

    fn var(&mut self) -> Id {
        let id = self.id();
        self.vars.insert(id);
        id
    }

    fn id(&mut self) -> Id {
        let id = self.next_id;
        self.next_id.0 += 1;
        id
    }
}
