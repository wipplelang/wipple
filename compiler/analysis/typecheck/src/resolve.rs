use crate::{Driver, Role, UnknownTypeId};
use derivative::Derivative;
use std::{
    cell::{Cell, RefCell},
    collections::{btree_map, BTreeMap, HashMap, HashSet},
    fmt::Debug,
    rc::Rc,
};
use wipple_util::WithInfo;

pub struct ItemDeclarationInner<D: Driver> {
    bounds: Vec<WithInfo<D::Info, crate::Instance<D>>>,
    r#type: WithInfo<D::Info, crate::Type<D>>,
    body: WithInfo<D::Info, crate::UntypedExpression<D>>,
    top_level: bool,
}

impl<D: Driver> crate::IntoItemDeclaration<D>
    for (
        WithInfo<D::Info, crate::ConstantDeclaration<D>>,
        WithInfo<D::Info, crate::UntypedExpression<D>>,
    )
{
    fn into_item_declaration(self, _driver: &D) -> WithInfo<D::Info, crate::ItemDeclaration<D>> {
        let (declaration, body) = self;

        declaration.map(|declaration| {
            crate::ItemDeclaration(ItemDeclarationInner {
                bounds: declaration.bounds,
                r#type: declaration.r#type,
                body,
                top_level: false,
            })
        })
    }
}

impl<D: Driver> crate::IntoItemDeclaration<D>
    for (
        WithInfo<D::Info, crate::InstanceDeclaration<D>>,
        WithInfo<D::Info, crate::UntypedExpression<D>>,
    )
{
    fn into_item_declaration(self, driver: &D) -> WithInfo<D::Info, crate::ItemDeclaration<D>> {
        let (declaration, body) = self;

        declaration.map(|declaration| {
            let r#type = resolve_trait_type_from_instance(driver, declaration.instance.as_ref());

            crate::ItemDeclaration(ItemDeclarationInner {
                bounds: declaration.bounds,
                r#type,
                body,
                top_level: false,
            })
        })
    }
}

impl<D: Driver> crate::IntoItemDeclaration<D>
    for WithInfo<D::Info, Vec<WithInfo<D::Info, crate::UntypedExpression<D>>>>
{
    fn into_item_declaration(self, driver: &D) -> WithInfo<D::Info, crate::ItemDeclaration<D>> {
        let info = self.info.clone();

        self.map(|code| {
            crate::ItemDeclaration(ItemDeclarationInner {
                bounds: Vec::new(),
                r#type: WithInfo {
                    info: info.clone(),
                    item: crate::Type::Unknown(UnknownTypeId::none()), // the top level can be any type
                },
                body: WithInfo {
                    info: driver.top_level_info(),
                    item: crate::UntypedExpression::Block(code),
                },
                top_level: true,
            })
        })
    }
}

pub fn resolve<D: Driver>(
    driver: &D,
    item_declaration: impl crate::IntoItemDeclaration<D>,
) -> crate::Result<D> {
    struct Queued<D: Driver> {
        use_info: D::Info,
        type_context: TypeContext<D>,
        bounds: RefCell<Vec<Vec<WithInfo<D::Info, Instance<D>>>>>,
        body: WithInfo<D::Info, Expression<D>>,
    }

    let item_declaration = item_declaration
        .into_item_declaration(driver)
        .map(|declaration| declaration.0);

    let recursion_stack: RefCell<Vec<_>> = Default::default();
    let recursion_limit = driver.recursion_limit();

    let error_queue: RefCell<Vec<_>> = Default::default();
    let errors: RefCell<Vec<_>> = Default::default();

    let type_context = TypeContext::default();

    let variables: RefCell<HashMap<_, _>> = Default::default();
    let infer_context = InferContext {
        driver,
        type_context: &type_context,
        error_queue: &error_queue,
        errors: &errors,
        variables: &variables,
    };

    let declared_type = infer_type(
        item_declaration.item.r#type.as_ref(),
        item_declaration.replace(Role::Annotation),
        Some(&type_context),
    );

    let body = infer_expression(item_declaration.item.body, infer_context);

    let bounds = vec![item_declaration
        .item
        .bounds
        .into_iter()
        .map(infer_instance)
        .collect()];

    let mut queued = Queued {
        use_info: item_declaration.info,
        type_context,
        bounds: RefCell::new(bounds),
        body,
    };

    let mut prev_subexpression_types = Vec::new();
    let item = loop {
        if recursion_stack.borrow().len() as u32 > recursion_limit {
            errors.borrow_mut().push(WithInfo {
                info: queued.body.info.clone(),
                item: crate::Diagnostic::RecursionLimit,
            });

            let finalize_context = FinalizeContext {
                driver,
                type_context: &queued.type_context,
                bound_instances: queued.bounds.clone(),
                error_queue: None,
                errors: None,
                unresolved_variables: None,
                contains_unknown: &Cell::new(false),
                subexpression_types: None,
            };

            break finalize_expression(queued.body, &finalize_context);
        }

        let resolve_context = ResolveContext {
            driver,
            type_context: &queued.type_context,
            error_queue: &error_queue,
            errors: &errors,
            variables: &variables,
            recursion_stack: &recursion_stack,
            bound_instances: queued.bounds.clone(),
        };

        try_unify_expression(
            driver,
            queued.body.as_mut(),
            &declared_type,
            &queued.type_context,
            &error_queue,
        );

        queued.body = resolve_expression(queued.body, &resolve_context);

        let subexpression_types = {
            let subexpression_types: RefCell<Vec<_>> = Default::default();

            let finalize_context = FinalizeContext {
                driver,
                type_context: &queued.type_context,
                bound_instances: queued.bounds.clone(),
                error_queue: None,
                errors: None,
                unresolved_variables: None,
                contains_unknown: &Cell::new(false),
                subexpression_types: Some(&subexpression_types),
            };

            let item = finalize_expression(queued.body.clone(), &finalize_context);

            if finalize_context.contains_unknown.get() {
                break item;
            }

            subexpression_types.into_inner()
        };

        let made_progress = subexpression_types != prev_subexpression_types;

        if !made_progress {
            let substituted_defaults =
                substitute_defaults_in_expression(driver, queued.body.as_mut(), &resolve_context);

            if !substituted_defaults {
                if item_declaration.item.top_level {
                    // The top level has type `()` by default, but any other type is also OK
                    let _ = unify(
                        driver,
                        &queued.body.item.r#type,
                        &Type::new(
                            TypeKind::Tuple(Vec::new()),
                            queued.body.info.clone(),
                            Vec::new(),
                        ),
                        &queued.type_context,
                    );
                }

                let unresolved_variables: RefCell<HashSet<_>> = Default::default();

                let finalize_context = FinalizeContext {
                    driver,
                    type_context: &queued.type_context,
                    bound_instances: queued.bounds.clone(),
                    error_queue: Some(&error_queue),
                    errors: Some(&errors),
                    unresolved_variables: Some(&unresolved_variables),
                    contains_unknown: &Cell::new(false),
                    subexpression_types: None,
                };

                break finalize_expression(queued.body, &finalize_context);
            }
        }

        prev_subexpression_types = subexpression_types;
        recursion_stack.borrow_mut().push(queued.use_info.clone());
    };

    let mut errors = errors.into_inner();
    report_queued_errors(
        driver,
        &queued.type_context,
        error_queue.into_inner(),
        &mut errors,
    );

    crate::Result {
        item,
        diagnostics: errors,
    }
}

pub fn instances_overlap<D: Driver>(
    driver: &D,
    r#trait: &D::Path,
    instances: impl IntoIterator<Item = D::Path>,
) -> Vec<WithInfo<D::Info, crate::Diagnostic<D>>> {
    let trait_declaration = driver.get_trait_declaration(r#trait);
    let opaque_parameters = trait_declaration
        .item
        .parameters
        .iter()
        .filter_map(|parameter| {
            let parameter_declaration = driver.get_type_parameter_declaration(parameter);
            parameter_declaration.item.infer.map(|_| parameter)
        })
        .collect::<HashSet<_>>();

    let instances = instances
        .into_iter()
        .map(|path| {
            let declaration = driver.get_instance_declaration(&path);

            (
                path,
                declaration.info,
                declaration.item.parameters,
                infer_instance(declaration.item.instance),
            )
        })
        .collect::<Vec<_>>();

    let mut errors = Vec::new();
    let mut overlapping = HashSet::new();

    for (index, (path, info, parameters, instance)) in instances.iter().enumerate() {
        for (other_index, (other_path, _, other_parameters, other_instance)) in
            instances.iter().enumerate()
        {
            if index == other_index
                || overlapping.contains(&(index, other_index))
                || overlapping.contains(&(other_index, index))
            {
                continue;
            }

            let type_context = TypeContext::default();

            let instantiate_instance =
                |parameters: Vec<<D as Driver>::Path>,
                 instance: WithInfo<D::Info, &mut Instance<D>>| {
                    let unused_errors = RefCell::default();

                    let instantiation_context = InstantiationContext::from_parameters(
                        driver,
                        parameters,
                        &type_context,
                        &instance.info,
                        &unused_errors,
                    );

                    instance
                        .item
                        .instantiate_mut(driver, &instantiation_context);

                    // Only count non-inferred parameters in the overlap check
                    for (trait_parameter, instance_parameter) in trait_declaration
                        .item
                        .parameters
                        .iter()
                        .zip(&mut instance.item.parameters)
                    {
                        if opaque_parameters.contains(trait_parameter) {
                            instance_parameter.kind = TypeKind::Opaque(type_context.variable());
                        }
                    }
                };

            let mut instance = instance.clone();

            instantiate_instance(parameters.clone(), instance.as_mut());

            let mut other_instance = other_instance.clone();

            instantiate_instance(other_parameters.clone(), other_instance.as_mut());

            if unify_instance(
                driver,
                instance.as_ref(),
                other_instance.as_ref(),
                &type_context,
            ) {
                overlapping.insert((index, other_index));

                errors.push(WithInfo {
                    info: info.clone(),
                    item: crate::Diagnostic::OverlappingInstances {
                        instance: path.clone(),
                        other: other_path.clone(),
                    },
                });
            }
        }
    }

    errors
}

pub fn resolve_trait_type_from_instance<D: Driver>(
    driver: &D,
    instance: WithInfo<D::Info, &crate::Instance<D>>,
) -> WithInfo<D::Info, crate::Type<D>> {
    let trait_declaration = driver.get_trait_declaration(&instance.item.r#trait);

    let role = trait_declaration.replace(Role::Trait);

    let type_context = TypeContext::default();
    let errors: RefCell<Vec<_>> = Default::default();

    let instantiation_context = InstantiationContext::from_parameters(
        driver,
        trait_declaration.item.parameters.clone(),
        &type_context,
        &instance.info,
        &errors,
    );

    for (trait_parameter, instance_parameter) in trait_declaration
        .item
        .parameters
        .into_iter()
        .zip(&instance.item.parameters)
    {
        let r#type = instantiation_context.type_for_parameter(driver, &trait_parameter);

        assert!(unify(
            driver,
            &r#type,
            &infer_type(instance_parameter.as_ref(), role.clone(), None),
            &type_context,
        ));
    }

    let r#type = infer_type(
        trait_declaration.item.r#type.as_ref(),
        role,
        Some(&type_context),
    )
    .instantiate(driver, &instantiation_context);

    assert!(errors.into_inner().is_empty());

    let finalize_context = FinalizeContext {
        driver,
        type_context: &type_context,
        bound_instances: Default::default(),
        error_queue: None,
        errors: None,
        unresolved_variables: None,
        contains_unknown: &Cell::new(false),
        subexpression_types: None,
    };

    finalize_type(r#type, &finalize_context)
}

// Instead of reporting unification errors immediately, queue them and then
// report them all once all type information has been collected.
enum QueuedError<D: Driver> {
    RecursionLimit,

    Mismatch {
        actual: Type<D>,
        expected: Type<D>,
    },

    WrongNumberOfInputs {
        actual: u32,
        expected: u32,
    },

    UnresolvedInstance {
        instance: Instance<D>,
        candidates: Vec<D::Info>,
        stack: Vec<WithInfo<D::Info, Instance<D>>>,
    },

    NotAStructure(Type<D>),

    MissingFields(Vec<String>),

    ExtraField,
}

fn report_queued_errors<D: Driver>(
    driver: &D,
    type_context: &TypeContext<D>,
    error_queue: Vec<WithInfo<D::Info, QueuedError<D>>>,
    errors: &mut Vec<WithInfo<D::Info, crate::Diagnostic<D>>>,
) {
    let finalize_context = FinalizeContext {
        driver,
        type_context,
        bound_instances: Default::default(),
        error_queue: None,
        errors: None,
        unresolved_variables: None,
        contains_unknown: &Cell::new(false),
        subexpression_types: None,
    };

    errors.extend(error_queue.into_iter().map(|error| {
        error.map(|error| match error {
            QueuedError::RecursionLimit => crate::Diagnostic::RecursionLimit,
            QueuedError::Mismatch { actual, expected } => crate::Diagnostic::Mismatch {
                actual_roles: actual.roles.clone(),
                actual: finalize_type(actual, &finalize_context),
                expected_roles: expected.roles.clone(),
                expected: finalize_type(expected, &finalize_context),
            },
            QueuedError::WrongNumberOfInputs { actual, expected } => {
                crate::Diagnostic::WrongNumberOfInputs { actual, expected }
            }
            QueuedError::UnresolvedInstance {
                instance,
                candidates,
                stack,
            } => crate::Diagnostic::UnresolvedInstance {
                instance: finalize_instance(instance, &finalize_context),
                candidates,
                stack: stack
                    .into_iter()
                    .map(|instance| {
                        instance.map(|instance| finalize_instance(instance, &finalize_context))
                    })
                    .collect(),
            },
            QueuedError::NotAStructure(r#type) => {
                crate::Diagnostic::NotAStructure(finalize_type(r#type, &finalize_context))
            }
            QueuedError::MissingFields(fields) => crate::Diagnostic::MissingFields(fields),
            QueuedError::ExtraField => crate::Diagnostic::ExtraField,
        })
    }));
}

// region: Types and type variables

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
struct Type<D: Driver> {
    kind: TypeKind<D>,
    info: D::Info,
    roles: Vec<WithInfo<D::Info, Role>>,
}

impl<D: Driver> Type<D> {
    fn new(kind: TypeKind<D>, info: D::Info, roles: Vec<WithInfo<D::Info, Role>>) -> Self {
        Type { kind, info, roles }
    }

    fn with_role(self, role: WithInfo<D::Info, Role>) -> Self {
        self.with_roles(std::iter::once(role))
    }

    fn with_roles(mut self, roles: impl IntoIterator<Item = WithInfo<D::Info, Role>>) -> Self {
        self.roles.extend(roles);
        self
    }

    fn contains_variable(&self, variable: &TypeVariable<D>) -> bool {
        match &self.kind {
            TypeKind::Variable(var) => var.counter == variable.counter,
            TypeKind::Declared { parameters, .. } => parameters
                .iter()
                .any(|r#type| r#type.contains_variable(variable)),
            TypeKind::Function { inputs, output } => {
                inputs
                    .iter()
                    .any(|r#type| r#type.contains_variable(variable))
                    || output.contains_variable(variable)
            }
            TypeKind::Tuple(elements) => elements
                .iter()
                .any(|r#type| r#type.contains_variable(variable)),
            TypeKind::Block(r#type) => r#type.contains_variable(variable),
            TypeKind::Opaque(_)
            | TypeKind::Parameter(_)
            | TypeKind::Unknown
            | TypeKind::Intrinsic => false,
        }
    }

    #[must_use]
    fn apply_in_context(&self, context: &TypeContext<D>) -> Self {
        let mut r#type = self.clone();
        r#type.apply_in_context_mut(context);
        r#type
    }

    fn apply_in_context_mut(&mut self, context: &TypeContext<D>) {
        match &mut self.kind {
            TypeKind::Variable(variable) => {
                let r#type =
                    match variable.with_substitution_mut(context, |substitution| match substitution
                    {
                        btree_map::Entry::Vacant(_) => None,
                        btree_map::Entry::Occupied(entry) => Some(entry.get().borrow().clone()),
                    }) {
                        Some(r#type) => r#type,
                        _ => return,
                    };

                assert!(!r#type.contains_variable(variable), "recursive type");

                self.kind = r#type.kind;
                self.info = r#type.info;
                self.roles.extend(r#type.roles);
                self.apply_in_context_mut(context);
            }
            TypeKind::Opaque(_) => {}
            TypeKind::Parameter(_) => {}
            TypeKind::Declared { parameters, .. } => {
                for r#type in parameters {
                    r#type.apply_in_context_mut(context);
                }
            }
            TypeKind::Function { inputs, output } => {
                for r#type in inputs {
                    r#type.apply_in_context_mut(context);
                }

                output.apply_in_context_mut(context);
            }
            TypeKind::Tuple(elements) => {
                for r#type in elements {
                    r#type.apply_in_context_mut(context);
                }
            }
            TypeKind::Block(r#type) => {
                r#type.apply_in_context_mut(context);
            }
            TypeKind::Unknown | TypeKind::Intrinsic => {}
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
enum TypeKind<D: Driver> {
    Variable(TypeVariable<D>),
    Opaque(TypeVariable<D>),
    Parameter(D::Path),
    Declared {
        path: D::Path,
        parameters: Vec<Type<D>>,
    },
    Function {
        inputs: Vec<Type<D>>,
        output: Box<Type<D>>,
    },
    Tuple(Vec<Type<D>>),
    Block(Box<Type<D>>),
    Intrinsic,
    Unknown,
}

#[derive(Derivative)]
#[derivative(
    Copy(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
struct TypeVariable<D: Driver> {
    _driver: std::marker::PhantomData<D>,
    counter: u32,
}

impl<D: Driver> Clone for TypeVariable<D> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<D: Driver> Debug for TypeVariable<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TypeVariable({})", self.counter)
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
struct Instance<D: Driver> {
    r#trait: D::Path,
    parameters: Vec<Type<D>>,
}

// region: Type context

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Default(bound = ""))]
struct TypeContext<D: Driver> {
    next_variable: Cell<u32>,
    substitutions: RefCell<BTreeMap<u32, Rc<RefCell<Type<D>>>>>,
    defaults: RefCell<BTreeMap<u32, Rc<RefCell<Type<D>>>>>,
}

impl<D: Driver> TypeContext<D> {
    fn replace_with(&self, other: Self) {
        self.next_variable.set(other.next_variable.get());
        self.substitutions.replace(other.substitutions.into_inner());
        self.defaults.replace(other.defaults.into_inner());
    }
}

// region: Instantiation context

struct InstantiationContext<'a, D: Driver> {
    type_context: &'a TypeContext<D>,
    types: Vec<(D::Path, Type<D>)>,
    info: &'a D::Info,
    errors: &'a RefCell<Vec<WithInfo<D::Info, crate::Diagnostic<D>>>>,
}

#[derive(Clone, Copy, Default)]
#[non_exhaustive]
struct InstantiationOptions {
    instantiate_inferred_parameters_as_opaque: bool,
}

impl<'a, D: Driver> InstantiationContext<'a, D> {
    pub fn from_parameters(
        driver: &D,
        parameters: impl IntoIterator<Item = D::Path>,
        type_context: &'a TypeContext<D>,
        info: &'a D::Info,
        errors: &'a RefCell<Vec<WithInfo<D::Info, crate::Diagnostic<D>>>>,
    ) -> Self {
        InstantiationContext::from_parameters_with_options(
            driver,
            parameters,
            type_context,
            info,
            errors,
            Default::default(),
        )
    }

    pub fn from_parameters_with_options(
        driver: &D,
        parameters: impl IntoIterator<Item = D::Path>,
        type_context: &'a TypeContext<D>,
        info: &'a D::Info,
        errors: &'a RefCell<Vec<WithInfo<D::Info, crate::Diagnostic<D>>>>,
        options: InstantiationOptions,
    ) -> Self {
        let mut context = InstantiationContext {
            type_context,
            types: Vec::new(),
            info,
            errors,
        };

        for path in parameters {
            let parameter_declaration = driver.get_type_parameter_declaration(&path);

            let variable = type_context.variable_with_default(
                parameter_declaration
                    .as_ref()
                    .item
                    .default
                    .as_ref()
                    .map(|r#type| {
                        infer_type(
                            r#type.as_ref(),
                            parameter_declaration.replace(Role::TypeParameter),
                            Some(type_context),
                        )
                        .instantiate(driver, &context)
                    }),
            );

            let kind = if parameter_declaration.item.infer.is_some()
                && options.instantiate_inferred_parameters_as_opaque
            {
                TypeKind::Opaque(variable)
            } else {
                TypeKind::Variable(variable)
            };

            context.types.push((
                path,
                Type::new(
                    kind,
                    parameter_declaration.info.clone(),
                    vec![parameter_declaration.replace(Role::TypeParameter)],
                ),
            ));
        }

        context
    }

    pub fn type_for_parameter(&self, driver: &D, parameter: &D::Path) -> Type<D> {
        self.types
            .iter()
            .find_map(|(instantiation_path, r#type)| {
                driver
                    .paths_are_equal(parameter, instantiation_path)
                    .then_some(r#type)
            })
            .cloned()
            .unwrap_or_else(|| {
                self.errors.borrow_mut().push(WithInfo {
                    info: self.info.clone(),
                    item: crate::Diagnostic::UndeclaredTypeParameter(parameter.clone()),
                });

                Type::new(TypeKind::Unknown, self.info.clone(), Vec::new())
            })
    }

    pub fn into_types_for_parameters(self) -> Vec<Type<D>> {
        self.types.into_iter().map(|(_, r#type)| r#type).collect()
    }
}

impl<D: Driver> Type<D> {
    #[must_use]
    fn instantiate(&self, driver: &D, instantiation_context: &InstantiationContext<'_, D>) -> Self {
        let mut r#type = self.clone();
        r#type.instantiate_mut(driver, instantiation_context);
        r#type
    }

    fn instantiate_mut(&mut self, driver: &D, instantiation_context: &InstantiationContext<'_, D>) {
        self.apply_in_context_mut(instantiation_context.type_context);

        match &mut self.kind {
            TypeKind::Variable(_) => {}
            TypeKind::Opaque(_) => {}
            TypeKind::Parameter(path) => {
                *self = instantiation_context.type_for_parameter(driver, path);
            }
            TypeKind::Declared { parameters, .. } => {
                for r#type in parameters {
                    r#type.instantiate_mut(driver, instantiation_context);
                }
            }
            TypeKind::Function { inputs, output } => {
                for r#type in inputs {
                    r#type.instantiate_mut(driver, instantiation_context);
                }

                output.instantiate_mut(driver, instantiation_context);
            }
            TypeKind::Tuple(elements) => {
                for r#type in elements {
                    r#type.instantiate_mut(driver, instantiation_context);
                }
            }
            TypeKind::Block(r#type) => {
                r#type.instantiate_mut(driver, instantiation_context);
            }
            TypeKind::Unknown | TypeKind::Intrinsic => {}
        }
    }

    #[must_use]
    fn instantiate_opaque_in_context(&self, context: &TypeContext<D>) -> Self {
        let mut r#type = self.clone();
        r#type.instantiate_opaque_in_context_mut(context);
        r#type
    }

    fn instantiate_opaque_in_context_mut(&mut self, context: &TypeContext<D>) {
        self.apply_in_context_mut(context);

        match &mut self.kind {
            TypeKind::Variable(_) => {}
            TypeKind::Opaque(variable) => {
                self.kind = TypeKind::Variable(*variable);
            }
            TypeKind::Parameter(_) => {}
            TypeKind::Declared { parameters, .. } => {
                for r#type in parameters {
                    r#type.instantiate_opaque_in_context_mut(context);
                }
            }
            TypeKind::Function { inputs, output } => {
                for r#type in inputs {
                    r#type.instantiate_opaque_in_context_mut(context);
                }

                output.instantiate_opaque_in_context_mut(context);
            }
            TypeKind::Tuple(elements) => {
                for r#type in elements {
                    r#type.instantiate_opaque_in_context_mut(context);
                }
            }
            TypeKind::Block(r#type) => {
                r#type.instantiate_opaque_in_context_mut(context);
            }
            TypeKind::Unknown | TypeKind::Intrinsic => {}
        }
    }
}

impl<D: Driver> Instance<D> {
    #[must_use]
    fn instantiate(&self, driver: &D, instantiation_context: &InstantiationContext<'_, D>) -> Self {
        let mut bound = self.clone();
        bound.instantiate_mut(driver, instantiation_context);
        bound
    }

    fn instantiate_mut(&mut self, driver: &D, instantiation_context: &InstantiationContext<'_, D>) {
        for parameter in &mut self.parameters {
            parameter.instantiate_mut(driver, instantiation_context);
        }
    }

    #[must_use]
    fn instantiate_opaque(&self, context: &TypeContext<D>) -> Self {
        let mut bound = self.clone();
        bound.instantiate_opaque_mut(context);
        bound
    }

    fn instantiate_opaque_mut(&mut self, context: &TypeContext<D>) {
        for parameter in &mut self.parameters {
            parameter.instantiate_opaque_in_context_mut(context);
        }
    }
}

// region: Unification

impl<D: Driver> TypeContext<D> {
    pub fn variable(&self) -> TypeVariable<D> {
        self.variable_with_default(None)
    }

    pub fn variable_with_default(&self, default: impl Into<Option<Type<D>>>) -> TypeVariable<D> {
        let counter = self.next_variable.get();
        self.next_variable.set(counter + 1);

        if let Some(default) = default.into() {
            self.defaults
                .borrow_mut()
                .insert(counter, Rc::new(RefCell::new(default)));
        }

        TypeVariable {
            _driver: std::marker::PhantomData,
            counter,
        }
    }
}

impl<D: Driver> TypeVariable<D> {
    fn with_substitution_mut<T>(
        &self,
        context: &TypeContext<D>,
        f: impl FnOnce(btree_map::Entry<'_, u32, Rc<RefCell<Type<D>>>>) -> T,
    ) -> T {
        f(context.substitutions.borrow_mut().entry(self.counter))
    }

    fn default(&self, context: &TypeContext<D>) -> Option<Type<D>> {
        context
            .defaults
            .borrow()
            .get(&self.counter)
            .map(|r#type| r#type.borrow().clone())
    }
}

#[derive(Clone, Copy, Default)]
#[non_exhaustive]
struct UnifyOptions {
    require_equal_type_parameters: bool,
}

#[must_use]
fn unify_with_options<D: Driver>(
    driver: &D,
    r#type: &Type<D>,
    expected_type: &Type<D>,
    context: &TypeContext<D>,
    options: UnifyOptions,
) -> bool {
    fn unify_variable<D: Driver>(
        variable: &TypeVariable<D>,
        r#type: &Type<D>,
        context: &TypeContext<D>,
    ) -> bool {
        if r#type.contains_variable(variable) {
            return false;
        }

        variable.with_substitution_mut(context, |substitution| match substitution {
            btree_map::Entry::Vacant(entry) => {
                entry.insert(Rc::new(RefCell::new(r#type.clone())));
            }
            btree_map::Entry::Occupied(_) => panic!("variable already has substitution"),
        });

        if let TypeKind::Variable(other) = &r#type.kind {
            if let Some(default) = variable.default(context) {
                context
                    .defaults
                    .borrow_mut()
                    .insert(other.counter, Rc::new(RefCell::new(default)));
            }
        }

        true
    }

    fn unify_inner<D: Driver>(
        driver: &D,
        r#type: &Type<D>,
        expected_type: &Type<D>,
        context: &TypeContext<D>,
        options: UnifyOptions,
    ) -> bool {
        let r#type = r#type.apply_in_context(context);
        let expected_type = expected_type.apply_in_context(context);

        match (&r#type.kind, &expected_type.kind) {
            // Opaque types don't unify with anything
            (TypeKind::Opaque(_), _) | (_, TypeKind::Opaque(_)) => true,

            // Type variables unify with anything and are substituted with the
            // other type in `apply_in_context`
            (TypeKind::Variable(variable), _) => {
                unify_variable(variable, &expected_type, context);
                true
            }
            (_, TypeKind::Variable(variable)) => {
                unify_variable(variable, &r#type, context);
                true
            }

            // Type parameters are equal to themselves, but otherwise must be
            // instantiated
            (TypeKind::Parameter(parameter), TypeKind::Parameter(expected_parameter)) => {
                driver.paths_are_equal(parameter, expected_parameter)
                    || !options.require_equal_type_parameters
            }
            (_, TypeKind::Parameter(_)) | (TypeKind::Parameter(_), _) => false,

            // Unify declared types, functions, tuples, and blocks structurally
            (
                TypeKind::Declared { path, parameters },
                TypeKind::Declared {
                    path: expected_path,
                    parameters: expected_parameters,
                },
            ) => {
                if !driver.paths_are_equal(path, expected_path) {
                    return false;
                }

                assert_eq!(parameters.len(), expected_parameters.len());
                let mut unified = true;
                for (r#type, expected_type) in parameters.iter().zip(expected_parameters) {
                    unified &= unify_inner(driver, r#type, expected_type, context, options);
                }

                unified
            }
            (
                TypeKind::Function { inputs, output },
                TypeKind::Function {
                    inputs: expected_inputs,
                    output: expected_output,
                },
            ) => {
                if inputs.len() != expected_inputs.len() {
                    return false;
                }

                let mut unified = true;
                for (r#type, expected_type) in inputs.iter().zip(expected_inputs) {
                    unified &= unify_inner(driver, r#type, expected_type, context, options);
                }

                unified & unify_inner(driver, output, expected_output, context, options)
            }
            (TypeKind::Tuple(elements), TypeKind::Tuple(expected_elements)) => {
                if elements.len() != expected_elements.len() {
                    return false;
                }

                let mut unified = true;
                for (r#type, expected_type) in elements.iter().zip(expected_elements) {
                    unified &= unify_inner(driver, r#type, expected_type, context, options);
                }

                unified
            }
            (TypeKind::Block(r#type), TypeKind::Block(expected_type)) => {
                unify_inner(driver, r#type, expected_type, context, options)
            }

            // Intrinsic types unify with other intrinsic types (they're
            // supposed to be wrapped in another type)
            (TypeKind::Intrinsic, TypeKind::Intrinsic) => true,

            // Unknown types unify with everything
            (TypeKind::Unknown, _) | (_, TypeKind::Unknown) => true,

            // Any other combination of types is an error
            _ => false,
        }
    }

    unify_inner(driver, r#type, expected_type, context, options)
}

#[must_use]
fn unify<D: Driver>(
    driver: &D,
    r#type: &Type<D>,
    expected_type: &Type<D>,
    context: &TypeContext<D>,
) -> bool {
    unify_with_options(
        driver,
        r#type,
        expected_type,
        context,
        UnifyOptions::default(),
    )
}

#[must_use]
fn unify_parameters_with_options<D: Driver>(
    driver: &D,
    parameters: &[Type<D>],
    expected_parameters: &[Type<D>],
    context: &TypeContext<D>,
    options: UnifyOptions,
) -> bool {
    let mut unified = true;
    for (r#type, expected_type) in parameters.iter().zip(expected_parameters) {
        unified &= unify_with_options(driver, r#type, expected_type, context, options);
    }

    unified
}

#[must_use]
fn unify_instance_with_options<D: Driver>(
    driver: &D,
    instance: WithInfo<D::Info, &Instance<D>>,
    expected_instance: WithInfo<D::Info, &Instance<D>>,
    context: &TypeContext<D>,
    options: UnifyOptions,
) -> bool {
    driver.paths_are_equal(&instance.item.r#trait, &expected_instance.item.r#trait)
        && unify_parameters_with_options(
            driver,
            &instance.item.parameters,
            &expected_instance.item.parameters,
            context,
            options,
        )
}

#[must_use]
fn unify_instance<D: Driver>(
    driver: &D,
    actual_instance: WithInfo<D::Info, &Instance<D>>,
    expected_instance: WithInfo<D::Info, &Instance<D>>,
    context: &TypeContext<D>,
) -> bool {
    unify_instance_with_options(
        driver,
        actual_instance,
        expected_instance,
        context,
        UnifyOptions::default(),
    )
}

fn try_unify_expression<D: Driver>(
    driver: &D,
    expression: WithInfo<D::Info, &mut Expression<D>>,
    expected_type: &Type<D>,
    context: &TypeContext<D>,
    error_queue: &RefCell<Vec<WithInfo<D::Info, QueuedError<D>>>>,
) {
    if !unify(driver, &expression.item.r#type, expected_type, context) {
        error_queue.borrow_mut().push(WithInfo {
            info: expression.info.clone(),
            item: QueuedError::Mismatch {
                actual: expression.item.r#type.clone(),
                expected: expected_type.clone(),
            },
        });
    }
}

fn try_unify<D: Driver>(
    driver: &D,
    r#type: WithInfo<D::Info, &Type<D>>,
    expected_type: &Type<D>,
    context: &TypeContext<D>,
    error_queue: &RefCell<Vec<WithInfo<D::Info, QueuedError<D>>>>,
) {
    if !unify(driver, r#type.item, expected_type, context) {
        error_queue.borrow_mut().push(WithInfo {
            info: r#type.info.clone(),
            item: QueuedError::Mismatch {
                actual: r#type.item.clone(),
                expected: expected_type.clone(),
            },
        });
    }
}

fn substitute_defaults<D: Driver>(
    driver: &D,
    r#type: &mut Type<D>,
    context: &TypeContext<D>,
) -> bool {
    r#type.apply_in_context_mut(context);

    match &mut r#type.kind {
        TypeKind::Variable(variable) => variable
            .default(context)
            .is_some_and(|default| unify(driver, r#type, &default, context)),
        TypeKind::Declared { parameters, .. } => {
            for r#type in parameters {
                if substitute_defaults(driver, r#type, context) {
                    return true;
                }
            }

            false
        }
        TypeKind::Function { inputs, output } => {
            for r#type in inputs {
                if substitute_defaults(driver, r#type, context) {
                    return true;
                }
            }

            substitute_defaults(driver, output, context)
        }
        TypeKind::Tuple(elements) => {
            for r#type in elements {
                if substitute_defaults(driver, r#type, context) {
                    return true;
                }
            }

            false
        }
        TypeKind::Block(r#type) => substitute_defaults(driver, r#type, context),
        TypeKind::Opaque(_) | TypeKind::Parameter(_) | TypeKind::Unknown | TypeKind::Intrinsic => {
            false
        }
    }
}

// region: Resolution

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
struct Expression<D: Driver> {
    r#type: Type<D>,
    kind: ExpressionKind<D>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
enum ExpressionKind<D: Driver> {
    Unknown(Option<D::Path>),
    Variable(String, D::Path),
    UnresolvedConstant(D::Path),
    UnresolvedTrait(D::Path),
    ResolvedConstant {
        path: D::Path,
        parameters: Vec<Type<D>>,
    },
    ResolvedTrait(D::Path),
    Number(String),
    Text(String),
    Block(Vec<WithInfo<D::Info, Expression<D>>>),
    Function {
        inputs: Vec<WithInfo<D::Info, crate::Pattern<D>>>,
        body: WithInfo<D::Info, Box<Expression<D>>>,
    },
    Call {
        function: WithInfo<D::Info, Box<Expression<D>>>,
        inputs: Vec<WithInfo<D::Info, Expression<D>>>,
    },
    When {
        input: WithInfo<D::Info, Box<Expression<D>>>,
        arms: Vec<WithInfo<D::Info, Arm<D>>>,
    },
    Intrinsic {
        name: String,
        inputs: Vec<WithInfo<D::Info, Expression<D>>>,
    },
    Initialize {
        pattern: WithInfo<D::Info, crate::Pattern<D>>,
        value: WithInfo<D::Info, Box<Expression<D>>>,
    },
    Marker(D::Path),
    UnresolvedStructure(Vec<WithInfo<D::Info, StructureFieldValue<D>>>),
    ResolvedStructure {
        structure: D::Path,
        fields: Vec<WithInfo<D::Info, StructureFieldValue<D>>>,
    },
    Variant {
        variant: WithInfo<D::Info, D::Path>,
        values: Vec<WithInfo<D::Info, Expression<D>>>,
    },
    Wrapper(WithInfo<D::Info, Box<Expression<D>>>),
    Tuple(Vec<WithInfo<D::Info, Expression<D>>>),
    Format {
        segments: Vec<FormatSegment<D>>,
        trailing: String,
    },
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
struct FormatSegment<D: Driver> {
    text: String,
    value: WithInfo<D::Info, Expression<D>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
struct StructureFieldValue<D: Driver> {
    name: String,
    value: WithInfo<D::Info, Expression<D>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
struct Arm<D: Driver> {
    pattern: WithInfo<D::Info, crate::Pattern<D>>,
    body: WithInfo<D::Info, Expression<D>>,
}

// region: Infer

struct InferContext<'a, D: Driver> {
    driver: &'a D,
    type_context: &'a TypeContext<D>,
    error_queue: &'a RefCell<Vec<WithInfo<D::Info, QueuedError<D>>>>,
    errors: &'a RefCell<Vec<WithInfo<D::Info, crate::Diagnostic<D>>>>,
    variables: &'a RefCell<HashMap<D::Path, Type<D>>>,
}

impl<'a, D: Driver> InferContext<'a, D> {
    fn from_resolve_context(context: &'a ResolveContext<'a, D>) -> Self {
        InferContext {
            driver: context.driver,
            type_context: context.type_context,
            error_queue: context.error_queue,
            errors: context.errors,
            variables: context.variables,
        }
    }
}

impl<'a, D: Driver> Clone for InferContext<'a, D> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, D: Driver> Copy for InferContext<'a, D> {}

fn infer_type<D: Driver>(
    r#type: WithInfo<D::Info, &crate::Type<D>>,
    role: impl Into<Option<WithInfo<D::Info, Role>>>,
    type_context: Option<&TypeContext<D>>,
) -> Type<D> {
    Type::new(
        match r#type.item {
            crate::Type::Parameter(path) => TypeKind::Parameter(path.clone()),
            crate::Type::Declared { path, parameters } => TypeKind::Declared {
                path: path.clone(),
                parameters: parameters
                    .iter()
                    .map(|r#type| infer_type(r#type.as_ref(), None, type_context))
                    .collect(),
            },
            crate::Type::Function { inputs, output } => TypeKind::Function {
                inputs: inputs
                    .iter()
                    .map(|r#type| infer_type(r#type.as_ref(), None, type_context))
                    .collect(),
                output: Box::new(infer_type(output.as_deref(), None, type_context)),
            },
            crate::Type::Tuple(elements) => TypeKind::Tuple(
                elements
                    .iter()
                    .map(|r#type| infer_type(r#type.as_ref(), None, type_context))
                    .collect(),
            ),
            crate::Type::Block(r#type) => {
                TypeKind::Block(Box::new(infer_type(r#type.as_deref(), None, type_context)))
            }
            crate::Type::Unknown(_) => match type_context {
                Some(type_context) => TypeKind::Variable(type_context.variable()),
                None => TypeKind::Unknown,
            },
            crate::Type::Intrinsic => TypeKind::Intrinsic,
        },
        r#type.info,
        Vec::from_iter(role.into()),
    )
}

fn infer_instance<D: Driver>(
    instance: WithInfo<D::Info, crate::Instance<D>>,
) -> WithInfo<D::Info, Instance<D>> {
    instance.map(|instance| Instance {
        r#trait: instance.r#trait,
        parameters: instance
            .parameters
            .into_iter()
            .map(|r#type| infer_type(r#type.as_ref(), None, None))
            .collect(),
    })
}

fn infer_expression<D: Driver>(
    expression: WithInfo<D::Info, crate::UntypedExpression<D>>,
    context: InferContext<'_, D>,
) -> WithInfo<<D as Driver>::Info, Expression<D>> {
    let info = expression.info.clone();

    let mut expression = expression.map(|expression| match expression {
        crate::UntypedExpression::Unknown => Expression {
            r#type: Type::new(TypeKind::Unknown, info.clone(), Vec::new()),
            kind: ExpressionKind::Unknown(None),
        },
        crate::UntypedExpression::Annotate { value, r#type } => {
            let mut value = infer_expression(value.unboxed(), context);

            let r#type = infer_type(
                r#type.as_ref(),
                value.replace(Role::Annotation),
                Some(context.type_context),
            );

            try_unify_expression(
                context.driver,
                value.as_mut(),
                &r#type,
                context.type_context,
                context.error_queue,
            );

            value.item
        }
        crate::UntypedExpression::Variable(name, variable) => {
            let r#type = context
                .variables
                .borrow()
                .get(&variable)
                .cloned()
                .unwrap_or_else(|| Type::new(TypeKind::Unknown, info.clone(), Vec::new()));

            Expression {
                r#type,
                kind: ExpressionKind::Variable(name, variable),
            }
        }
        crate::UntypedExpression::Constant(path) => {
            let constant_declaration = context.driver.get_constant_declaration(&path);

            let instantiation_context = InstantiationContext::from_parameters(
                context.driver,
                constant_declaration.item.parameters.clone(),
                context.type_context,
                &info,
                context.errors,
            );

            let r#type = infer_type(
                constant_declaration.item.r#type.as_ref(),
                constant_declaration.replace(Role::Annotation),
                Some(context.type_context),
            )
            .instantiate(context.driver, &instantiation_context);

            Expression {
                r#type,
                kind: ExpressionKind::UnresolvedConstant(path),
            }
        }
        crate::UntypedExpression::Trait(path) => {
            let trait_declaration = context.driver.get_trait_declaration(&path);

            let instantiation_context = InstantiationContext::from_parameters(
                context.driver,
                trait_declaration.item.parameters.clone(),
                context.type_context,
                &info,
                context.errors,
            );

            let r#type = infer_type(
                trait_declaration.item.r#type.as_ref(),
                trait_declaration.replace(Role::Trait),
                Some(context.type_context),
            )
            .instantiate(context.driver, &instantiation_context);

            Expression {
                r#type,
                kind: ExpressionKind::UnresolvedTrait(path),
            }
        }
        crate::UntypedExpression::Number(number) => {
            let r#type = instantiated_language_type(
                "number",
                &info,
                context.driver,
                context.type_context,
                context.errors,
            )
            .unwrap_or_else(|| {
                Type::new(
                    TypeKind::Variable(context.type_context.variable()),
                    info.clone(),
                    Vec::new(),
                )
            });

            Expression {
                r#type,
                kind: ExpressionKind::Number(number),
            }
        }
        crate::UntypedExpression::Text(text) => {
            let r#type = instantiated_language_type(
                "text",
                &info,
                context.driver,
                context.type_context,
                context.errors,
            )
            .unwrap_or_else(|| {
                Type::new(
                    TypeKind::Variable(context.type_context.variable()),
                    info.clone(),
                    Vec::new(),
                )
            });

            Expression {
                r#type,
                kind: ExpressionKind::Text(text),
            }
        }
        crate::UntypedExpression::Block(statements) => {
            let statement_count = statements.len();

            let statements = statements
                .into_iter()
                .enumerate()
                .map(|(index, expression)| {
                    let statement = infer_expression(expression, context);

                    if index + 1 < statement_count {
                        // Statements have type `()` by default, but any other type is also OK
                        let _ = unify(
                            context.driver,
                            &statement.item.r#type,
                            &Type::new(
                                TypeKind::Tuple(Vec::new()),
                                statement.info.clone(),
                                Vec::new(),
                            ),
                            context.type_context,
                        );
                    }

                    statement
                })
                .collect::<Vec<_>>();

            let r#type = statements.last().map_or_else(
                || {
                    Type::new(
                        TypeKind::Tuple(Vec::new()),
                        info.clone(),
                        vec![WithInfo {
                            info: info.clone(),
                            item: Role::EmptyBlock,
                        }],
                    )
                },
                |expression| expression.item.r#type.clone(),
            );

            Expression {
                r#type: Type::new(TypeKind::Block(Box::new(r#type)), info.clone(), Vec::new()),
                kind: ExpressionKind::Block(statements),
            }
        }
        crate::UntypedExpression::Function { inputs, body } => {
            let input_types = inputs
                .iter()
                .map(|_| {
                    Type::new(
                        TypeKind::Variable(context.type_context.variable()),
                        info.clone(),
                        Vec::new(),
                    )
                })
                .collect::<Vec<_>>();

            for (pattern, input_type) in inputs.iter().zip(&input_types) {
                resolve_pattern(pattern.as_ref(), pattern.replace(input_type), context);
            }

            let body = infer_expression(body.unboxed(), context);

            Expression {
                r#type: Type::new(
                    TypeKind::Function {
                        inputs: input_types,
                        output: Box::new(
                            body.item
                                .r#type
                                .clone()
                                .with_role(body.replace(Role::FunctionOutput)),
                        ),
                    },
                    info.clone(),
                    Vec::new(),
                ),
                kind: ExpressionKind::Function {
                    inputs,
                    body: body.boxed(),
                },
            }
        }
        crate::UntypedExpression::Call { function, inputs } => {
            let function = infer_expression(function.unboxed(), context);

            let inputs = inputs
                .into_iter()
                .map(|input| {
                    let mut input = infer_expression(input, context);
                    let role = input.replace(Role::FunctionInput);
                    input.item.r#type = input.item.r#type.with_role(role);
                    input
                })
                .collect::<Vec<_>>();

            let r#type = match &function.item.r#type.kind {
                TypeKind::Function { output, .. } => output.as_ref().clone(),
                _ => Type::new(
                    TypeKind::Variable(context.type_context.variable()),
                    info.clone(),
                    vec![function.replace(Role::FunctionOutput)],
                ),
            };

            Expression {
                r#type,
                kind: ExpressionKind::Call {
                    function: function.boxed(),
                    inputs,
                },
            }
        }
        crate::UntypedExpression::When { input, arms } => {
            let input = infer_expression(input.unboxed(), context);

            let r#type = Type::new(
                TypeKind::Variable(context.type_context.variable()),
                info.clone(),
                Vec::new(),
            );

            let arms = arms
                .into_iter()
                .map(|arm| {
                    arm.map(|arm| {
                        resolve_pattern(
                            arm.pattern.as_ref(),
                            input.as_ref().map(|input| &input.r#type),
                            context,
                        );

                        let mut arm = Arm {
                            pattern: arm.pattern,
                            body: infer_expression(arm.body, context),
                        };

                        try_unify_expression(
                            context.driver,
                            arm.body.as_mut(),
                            &r#type,
                            context.type_context,
                            context.error_queue,
                        );

                        arm
                    })
                })
                .collect();

            Expression {
                r#type,
                kind: ExpressionKind::When {
                    input: input.boxed(),
                    arms,
                },
            }
        }
        crate::UntypedExpression::Intrinsic { name, inputs } => Expression {
            r#type: Type::new(
                TypeKind::Variable(context.type_context.variable()),
                info.clone(),
                Vec::new(),
            ),
            kind: ExpressionKind::Intrinsic {
                name,
                inputs: inputs
                    .into_iter()
                    .map(|expression| infer_expression(expression, context))
                    .collect(),
            },
        },
        crate::UntypedExpression::Initialize { pattern, value } => {
            let value = infer_expression(value.unboxed(), context);

            resolve_pattern(
                pattern.as_ref(),
                value.as_ref().map(|value| &value.r#type),
                context,
            );

            Expression {
                r#type: Type::new(TypeKind::Tuple(Vec::new()), info.clone(), Vec::new()),
                kind: ExpressionKind::Initialize {
                    pattern,
                    value: value.boxed(),
                },
            }
        }
        crate::UntypedExpression::Marker(path) => {
            let type_declaration = context.driver.get_type_declaration(&path);

            let instantiation_context = InstantiationContext::from_parameters(
                context.driver,
                type_declaration.item.parameters.clone(),
                context.type_context,
                &info,
                context.errors,
            );

            let r#type = Type::new(
                TypeKind::Declared {
                    path: path.clone(),
                    parameters: type_declaration
                        .item
                        .parameters
                        .into_iter()
                        .map(|path| instantiation_context.type_for_parameter(context.driver, &path))
                        .collect(),
                },
                info.clone(),
                Vec::new(),
            );

            Expression {
                r#type,
                kind: ExpressionKind::Marker(path),
            }
        }
        crate::UntypedExpression::Structure(fields) => Expression {
            r#type: Type::new(
                TypeKind::Variable(context.type_context.variable()),
                info.clone(),
                Vec::new(),
            ),
            kind: ExpressionKind::UnresolvedStructure(
                fields
                    .into_iter()
                    .map(|field| {
                        field.map(|field| StructureFieldValue {
                            name: field.name,
                            value: infer_expression(field.value, context),
                        })
                    })
                    .collect(),
            ),
        },
        crate::UntypedExpression::Variant { variant, values } => {
            let enumeration = context.driver.get_enumeration_for_variant(&variant.item);
            let type_declaration = context.driver.get_type_declaration(&enumeration);

            let instantiation_context = InstantiationContext::from_parameters(
                context.driver,
                type_declaration.item.parameters.clone(),
                context.type_context,
                &info,
                context.errors,
            );

            let values = match type_declaration.item.representation.item {
                crate::TypeRepresentation::Enumeration(declared_variants) => {
                    let declared_variant = match declared_variants.get(&variant.item) {
                        Some(variant) => variant,
                        None => todo!("report error"),
                    };

                    values
                        .into_iter()
                        .zip(&declared_variant.item.value_types)
                        .map(|(value, declared_type)| {
                            let declared_type = infer_type(
                                declared_type.as_ref(),
                                declared_variant.replace(Role::VariantElement),
                                Some(context.type_context),
                            )
                            .instantiate(context.driver, &instantiation_context);

                            let mut value = infer_expression(value, context);

                            try_unify_expression(
                                context.driver,
                                value.as_mut(),
                                &declared_type,
                                context.type_context,
                                context.error_queue,
                            );

                            value
                        })
                        .collect()
                }
                _ => Vec::new(),
            };

            let r#type = Type::new(
                TypeKind::Declared {
                    path: enumeration,
                    parameters: type_declaration
                        .item
                        .parameters
                        .into_iter()
                        .map(|path| instantiation_context.type_for_parameter(context.driver, &path))
                        .collect(),
                },
                info.clone(),
                Vec::new(),
            );

            Expression {
                r#type,
                kind: ExpressionKind::Variant { variant, values },
            }
        }
        crate::UntypedExpression::Wrapper {
            r#type: path,
            value,
        } => {
            let type_declaration = context.driver.get_type_declaration(&path);

            let instantiation_context = InstantiationContext::from_parameters(
                context.driver,
                type_declaration.item.parameters.clone(),
                context.type_context,
                &info,
                context.errors,
            );

            let r#type = Type::new(
                TypeKind::Declared {
                    path,
                    parameters: type_declaration
                        .item
                        .parameters
                        .into_iter()
                        .map(|path| instantiation_context.type_for_parameter(context.driver, &path))
                        .collect(),
                },
                info.clone(),
                Vec::new(),
            );

            let value = match type_declaration.item.representation.item {
                crate::TypeRepresentation::Wrapper(declared_type) => {
                    let declared_type = infer_type(
                        declared_type.as_ref(),
                        declared_type.replace(Role::WrappedType),
                        Some(context.type_context),
                    )
                    .instantiate(context.driver, &instantiation_context);

                    let mut value = infer_expression(value.unboxed(), context);

                    try_unify_expression(
                        context.driver,
                        value.as_mut(),
                        &declared_type,
                        context.type_context,
                        context.error_queue,
                    );

                    value
                }
                _ => WithInfo {
                    info: info.clone(),
                    item: Expression {
                        r#type: Type::new(
                            TypeKind::Variable(context.type_context.variable()),
                            info.clone(),
                            Vec::new(),
                        ),
                        kind: ExpressionKind::Unknown(None),
                    },
                },
            };

            Expression {
                r#type,
                kind: ExpressionKind::Wrapper(value.boxed()),
            }
        }
        crate::UntypedExpression::Tuple(elements) => {
            let elements = elements
                .into_iter()
                .map(|expression| infer_expression(expression, context))
                .collect::<Vec<_>>();

            Expression {
                r#type: Type::new(
                    TypeKind::Tuple(
                        elements
                            .iter()
                            .map(|element| element.item.r#type.clone())
                            .collect(),
                    ),
                    info.clone(),
                    Vec::new(),
                ),
                kind: ExpressionKind::Tuple(elements),
            }
        }
        crate::UntypedExpression::Collection(elements) => {
            let element_type = Type::new(
                TypeKind::Variable(context.type_context.variable()),
                info.clone(),
                vec![WithInfo {
                    info: info.clone(),
                    item: Role::CollectionElement,
                }],
            );

            let initial_collection =
                instantiated_language_constant("initial-collection", &info, context);

            let collection_type = initial_collection.item.r#type.clone();

            elements
                .into_iter()
                .fold(initial_collection, |current, expression| {
                    let mut expression = infer_expression(expression, context);

                    try_unify_expression(
                        context.driver,
                        expression.as_mut(),
                        &element_type,
                        context.type_context,
                        context.error_queue,
                    );

                    expression.item.r#type = element_type.clone();

                    let build_collection_trait =
                        instantiated_language_trait("build-collection", &info, context);

                    WithInfo {
                        info: expression.info.clone(),
                        item: Expression {
                            r#type: collection_type.clone(),
                            kind: ExpressionKind::Call {
                                function: WithInfo {
                                    info: current.info.clone(),
                                    item: Box::new(Expression {
                                        r#type: Type::new(
                                            TypeKind::Function {
                                                inputs: vec![collection_type.clone()],
                                                output: Box::new(collection_type.clone()),
                                            },
                                            current.info.clone(),
                                            Vec::new(),
                                        ),
                                        kind: ExpressionKind::Call {
                                            function: build_collection_trait.boxed(),
                                            inputs: vec![expression],
                                        },
                                    }),
                                },
                                inputs: vec![current],
                            },
                        },
                    }
                })
                .item
        }
        crate::UntypedExpression::Format { segments, trailing } => {
            let text_type = instantiated_language_type(
                "text",
                &info,
                context.driver,
                context.type_context,
                context.errors,
            )
            .unwrap_or_else(|| {
                Type::new(
                    TypeKind::Variable(context.type_context.variable()),
                    info.clone(),
                    Vec::new(),
                )
            });

            let segments = segments
                .into_iter()
                .map(|segment| {
                    let value = infer_expression(segment.value, context);

                    let show_trait = instantiated_language_trait("show", &info, context);

                    FormatSegment {
                        text: segment.text,
                        value: WithInfo {
                            info: value.info.clone(),
                            item: Expression {
                                r#type: Type::new(
                                    TypeKind::Variable(context.type_context.variable()),
                                    value.info.clone(),
                                    Vec::new(),
                                ),
                                kind: ExpressionKind::Call {
                                    function: show_trait.boxed(),
                                    inputs: vec![value],
                                },
                            },
                        },
                    }
                })
                .collect();

            Expression {
                r#type: text_type,
                kind: ExpressionKind::Format { segments, trailing },
            }
        }
    });

    expression.item.r#type.info = info;

    expression
}

fn resolve_pattern<D: Driver>(
    pattern: WithInfo<D::Info, &crate::Pattern<D>>,
    r#type: WithInfo<D::Info, &Type<D>>,
    context: InferContext<'_, D>,
) {
    let mut r#type = r#type.map(|r#type| r#type.clone());
    r#type.item.apply_in_context_mut(context.type_context);

    match &pattern.item {
        crate::Pattern::Unknown => {}
        crate::Pattern::Wildcard => {}
        crate::Pattern::Number(_) => {
            let number_type = instantiated_language_type(
                "number",
                &pattern.info,
                context.driver,
                context.type_context,
                context.errors,
            )
            .map_or_else(
                || {
                    Type::new(
                        TypeKind::Variable(context.type_context.variable()),
                        pattern.info.clone(),
                        Vec::new(),
                    )
                },
                |r#type| r#type,
            );

            try_unify(
                context.driver,
                r#type.replace(&number_type),
                &r#type.item,
                context.type_context,
                context.error_queue,
            );
        }
        crate::Pattern::Text(_) => {
            let text_type = instantiated_language_type(
                "text",
                &pattern.info,
                context.driver,
                context.type_context,
                context.errors,
            )
            .map_or_else(
                || {
                    Type::new(
                        TypeKind::Variable(context.type_context.variable()),
                        pattern.info.clone(),
                        Vec::new(),
                    )
                },
                |r#type| r#type,
            );

            try_unify(
                context.driver,
                r#type.replace(&text_type),
                &r#type.item,
                context.type_context,
                context.error_queue,
            );
        }
        crate::Pattern::Variable(_, variable) => {
            use std::collections::hash_map::Entry;

            let mut variables = context.variables.borrow_mut();

            match variables.entry(variable.clone()) {
                Entry::Occupied(entry) => {
                    try_unify(
                        context.driver,
                        r#type.as_ref(),
                        entry.get(),
                        context.type_context,
                        context.error_queue,
                    );
                }
                Entry::Vacant(entry) => {
                    entry.insert(r#type.item);
                }
            }
        }
        crate::Pattern::Destructure(fields) => {
            let field_types = match &r#type.item.kind {
                TypeKind::Declared { path, parameters } => {
                    let type_declaration = context.driver.get_type_declaration(path);

                    let instantiation_context = InstantiationContext::from_parameters(
                        context.driver,
                        type_declaration.item.parameters.clone(),
                        context.type_context,
                        &pattern.info,
                        context.errors,
                    );

                    for (path, r#type) in type_declaration.item.parameters.iter().zip(parameters) {
                        assert!(unify(
                            context.driver,
                            &instantiation_context.type_for_parameter(context.driver, path),
                            r#type,
                            context.type_context,
                        ));
                    }

                    match type_declaration.item.representation.item {
                        crate::TypeRepresentation::Structure(field_types) => fields
                            .iter()
                            .map(|field| {
                                infer_type(
                                    field_types
                                        .get(&field.item.name)
                                        .unwrap()
                                        .item
                                        .r#type
                                        .as_ref(),
                                    field.replace(Role::StructureField),
                                    Some(context.type_context),
                                )
                                .instantiate(context.driver, &instantiation_context)
                            })
                            .collect::<Vec<_>>(),
                        _ => panic!("expected structure type"),
                    }
                }
                _ => fields
                    .iter()
                    .map(|field| {
                        Type::new(
                            TypeKind::Variable(context.type_context.variable()),
                            field.info.clone(),
                            Vec::new(),
                        )
                    })
                    .collect::<Vec<_>>(),
            };

            for (field, r#type) in fields.iter().zip(field_types) {
                resolve_pattern(field.item.pattern.as_ref(), field.replace(&r#type), context);
            }
        }
        crate::Pattern::Variant {
            variant,
            value_patterns,
        } => {
            let value_types = match &r#type.item.kind {
                TypeKind::Declared { path, parameters } => {
                    let type_declaration = context.driver.get_type_declaration(path);

                    let instantiation_context = InstantiationContext::from_parameters(
                        context.driver,
                        type_declaration.item.parameters.clone(),
                        context.type_context,
                        &pattern.info,
                        context.errors,
                    );

                    for (path, r#type) in type_declaration.item.parameters.iter().zip(parameters) {
                        assert!(unify(
                            context.driver,
                            &instantiation_context.type_for_parameter(context.driver, path),
                            r#type,
                            context.type_context,
                        ));
                    }

                    match &type_declaration.item.representation.item {
                        crate::TypeRepresentation::Enumeration(variants) => {
                            let variant = variants.get(&variant.item).unwrap();

                            variant
                                .item
                                .value_types
                                .clone()
                                .into_iter()
                                .map(|r#type| {
                                    infer_type(
                                        r#type.as_ref(),
                                        variant.replace(Role::VariantElement),
                                        Some(context.type_context),
                                    )
                                    .instantiate(context.driver, &instantiation_context)
                                })
                                .collect::<Vec<_>>()
                        }
                        _ => panic!("expected enumeration type"),
                    }
                }
                _ => {
                    let enumeration = context.driver.get_enumeration_for_variant(&variant.item);
                    let type_declaration = context.driver.get_type_declaration(&enumeration);

                    let instantiation_context = InstantiationContext::from_parameters(
                        context.driver,
                        type_declaration.item.parameters.clone(),
                        context.type_context,
                        &pattern.info,
                        context.errors,
                    );

                    let value_types = match type_declaration.item.representation.item {
                        crate::TypeRepresentation::Enumeration(variants) => {
                            let variant = variants.get(&variant.item).unwrap();

                            variant
                                .item
                                .value_types
                                .iter()
                                .map(|r#type| {
                                    infer_type(
                                        r#type.as_ref(),
                                        r#type.replace(Role::VariantElement),
                                        Some(context.type_context),
                                    )
                                    .instantiate(context.driver, &instantiation_context)
                                })
                                .collect::<Vec<_>>()
                        }
                        _ => panic!("expected enumeration type"),
                    };

                    let enumeration_type = Type::new(
                        TypeKind::Declared {
                            path: enumeration,
                            parameters: instantiation_context.into_types_for_parameters(),
                        },
                        pattern.info.clone(),
                        Vec::new(),
                    );

                    try_unify(
                        context.driver,
                        r#type.as_ref(),
                        &enumeration_type,
                        context.type_context,
                        context.error_queue,
                    );

                    value_types
                }
            };

            for (pattern, r#type) in value_patterns.iter().zip(value_types) {
                resolve_pattern(pattern.as_ref(), pattern.replace(&r#type), context);
            }
        }
        crate::Pattern::Tuple(elements) => {
            let element_types = match &r#type.item.kind {
                TypeKind::Tuple(element_types) => {
                    element_types.iter().map(Type::clone).collect::<Vec<_>>()
                }
                _ => elements
                    .iter()
                    .map(|element| {
                        Type::new(
                            TypeKind::Variable(context.type_context.variable()),
                            element.info.clone(),
                            Vec::new(),
                        )
                    })
                    .collect::<Vec<_>>(),
            };

            let tuple_type = Type::new(
                TypeKind::Tuple(element_types.iter().map(Type::clone).collect()),
                pattern.info.clone(),
                Vec::new(),
            );

            try_unify(
                context.driver,
                r#type.replace(&tuple_type),
                &r#type.item,
                context.type_context,
                context.error_queue,
            );

            for (element, r#type) in elements.iter().zip(element_types) {
                resolve_pattern(element.as_ref(), element.replace(&r#type), context);
            }
        }
        crate::Pattern::Or { left, right } => {
            resolve_pattern(left.as_deref(), r#type.as_ref(), context);
            resolve_pattern(right.as_deref(), r#type.as_ref(), context);
        }
    }
}

fn instantiated_language_type<D: Driver>(
    language_item: &'static str,
    info: &D::Info,
    driver: &D,
    type_context: &TypeContext<D>,
    errors: &RefCell<Vec<WithInfo<D::Info, crate::Diagnostic<D>>>>,
) -> Option<Type<D>> {
    match try_instantiated_language_type(language_item, info, driver, type_context, errors) {
        Some(path) => Some(path),
        None => {
            errors.borrow_mut().push(WithInfo {
                info: info.clone(),
                item: crate::Diagnostic::MissingLanguageItem(language_item.to_string()),
            });

            None
        }
    }
}

fn try_instantiated_language_type<D: Driver>(
    language_item: &'static str,
    info: &D::Info,
    driver: &D,
    type_context: &TypeContext<D>,
    errors: &RefCell<Vec<WithInfo<D::Info, crate::Diagnostic<D>>>>,
) -> Option<Type<D>> {
    let path = driver.path_for_language_type(language_item)?;
    let type_declaration = driver.get_type_declaration(&path);

    let role = type_declaration.replace(Role::Annotation);

    let instantiation_context = InstantiationContext::from_parameters(
        driver,
        type_declaration.item.parameters,
        type_context,
        info,
        errors,
    );

    Some(Type::new(
        TypeKind::Declared {
            path,
            parameters: instantiation_context.into_types_for_parameters(),
        },
        info.clone(),
        vec![role],
    ))
}

fn instantiated_language_trait<D: Driver>(
    language_item: &'static str,
    info: &D::Info,
    context: InferContext<'_, D>,
) -> WithInfo<D::Info, Expression<D>> {
    match context.driver.path_for_language_constructor(language_item) {
        Some(path) => infer_expression(
            WithInfo {
                info: info.clone(),
                item: crate::UntypedExpression::Constant(path),
            },
            context,
        ),
        None => {
            context.errors.borrow_mut().push(WithInfo {
                info: info.clone(),
                item: crate::Diagnostic::MissingLanguageItem(language_item.to_string()),
            });

            WithInfo {
                info: info.clone(),
                item: Expression {
                    r#type: Type::new(TypeKind::Unknown, info.clone(), Vec::new()),
                    kind: ExpressionKind::Unknown(None),
                },
            }
        }
    }
}

fn instantiated_language_constant<D: Driver>(
    language_item: &'static str,
    info: &D::Info,
    context: InferContext<'_, D>,
) -> WithInfo<D::Info, Expression<D>> {
    match context.driver.path_for_language_constant(language_item) {
        Some(path) => infer_expression(
            WithInfo {
                info: info.clone(),
                item: crate::UntypedExpression::Constant(path),
            },
            context,
        ),
        None => {
            context.errors.borrow_mut().push(WithInfo {
                info: info.clone(),
                item: crate::Diagnostic::MissingLanguageItem(language_item.to_string()),
            });

            WithInfo {
                info: info.clone(),
                item: Expression {
                    r#type: Type::new(TypeKind::Unknown, info.clone(), Vec::new()),
                    kind: ExpressionKind::Unknown(None),
                },
            }
        }
    }
}

// region: Resolve

struct ResolveContext<'a, D: Driver> {
    driver: &'a D,
    type_context: &'a TypeContext<D>,
    error_queue: &'a RefCell<Vec<WithInfo<D::Info, QueuedError<D>>>>,
    errors: &'a RefCell<Vec<WithInfo<D::Info, crate::Diagnostic<D>>>>,
    variables: &'a RefCell<HashMap<D::Path, Type<D>>>,
    recursion_stack: &'a RefCell<Vec<D::Info>>,
    bound_instances: RefCell<Vec<Vec<WithInfo<D::Info, Instance<D>>>>>,
}

fn resolve_expression<D: Driver>(
    mut expression: WithInfo<D::Info, Expression<D>>,
    context: &ResolveContext<'_, D>,
) -> WithInfo<D::Info, Expression<D>> {
    let kind = match expression.item.kind {
        ExpressionKind::Unknown(path) => ExpressionKind::Unknown(path),
        ExpressionKind::Variable(ref name, ref variable) => {
            let variable_type = context
                .variables
                .borrow()
                .get(variable)
                .cloned()
                .unwrap_or_else(|| {
                    Type::new(TypeKind::Unknown, expression.info.clone(), Vec::new())
                });

            let name = name.clone();
            let variable = variable.clone();

            try_unify_expression(
                context.driver,
                expression.as_mut(),
                &variable_type,
                context.type_context,
                context.error_queue,
            );

            ExpressionKind::Variable(name, variable)
        }
        ExpressionKind::UnresolvedConstant(ref path) => {
            let path = path.clone();

            match resolve_item(&path, expression.as_mut(), true, context) {
                Ok(Some(parameters)) => ExpressionKind::ResolvedConstant { path, parameters },
                Ok(None) => ExpressionKind::UnresolvedConstant(path), // try again with more type information
                Err(error) => {
                    context.error_queue.borrow_mut().push(error);
                    ExpressionKind::Unknown(Some(path))
                }
            }
        }
        ExpressionKind::UnresolvedTrait(ref path) => {
            let path = path.clone();

            let parameters =
                resolve_trait_parameters_from_type(&path, expression.as_mut(), context);

            let query = expression.replace(Instance {
                r#trait: path,
                parameters,
            });

            match resolve_trait(query.as_ref(), context) {
                Ok(()) => ExpressionKind::ResolvedTrait(query.item.r#trait),
                Err(error) => {
                    context.error_queue.borrow_mut().push(error);
                    ExpressionKind::Unknown(Some(query.item.r#trait))
                }
            }
        }
        ExpressionKind::ResolvedConstant { path, parameters } => {
            ExpressionKind::ResolvedConstant { path, parameters }
        }
        ExpressionKind::ResolvedTrait(path) => ExpressionKind::ResolvedTrait(path),
        ExpressionKind::Number(number) => ExpressionKind::Number(number),
        ExpressionKind::Text(text) => ExpressionKind::Text(text),
        ExpressionKind::Block(statements) => ExpressionKind::Block(
            statements
                .into_iter()
                .map(|expression| resolve_expression(expression, context))
                .collect(),
        ),
        ExpressionKind::Function { inputs, body } => {
            if let TypeKind::Function {
                inputs: input_types,
                ..
            } = &expression.item.r#type.kind
            {
                for (pattern, input_type) in inputs.iter().zip(input_types) {
                    resolve_pattern(
                        pattern.as_ref(),
                        pattern.replace(input_type),
                        InferContext::from_resolve_context(context),
                    );
                }
            }

            ExpressionKind::Function {
                inputs,
                body: resolve_expression(body.unboxed(), context).boxed(),
            }
        }
        ExpressionKind::Call {
            mut function,
            mut inputs,
        } => {
            function
                .item
                .r#type
                .apply_in_context_mut(context.type_context);

            for input in &mut inputs {
                input.item.r#type.apply_in_context_mut(context.type_context);
            }

            // If we encounter a unit after a number, treat the unit as a
            // function
            let function_is_number = (|| {
                let number_type = match try_instantiated_language_type(
                    "number",
                    &expression.info,
                    context.driver,
                    context.type_context,
                    context.errors,
                ) {
                    Some(number_type) => number_type,
                    None => return false,
                };

                let number_type_path = match number_type.kind {
                    TypeKind::Declared { path, .. } => path,
                    _ => return false,
                };

                matches!(
                    &function.item.r#type.kind,
                    TypeKind::Declared { path, .. }
                        if context.driver.paths_are_equal(path, &number_type_path),
                )
            })();

            if inputs.len() == 1 && function_is_number {
                let input = inputs.pop().unwrap();

                let mut unit = resolve_expression(input, context);
                let number = resolve_expression(function.unboxed(), context);

                let unit_type = Type::new(
                    TypeKind::Function {
                        inputs: vec![number
                            .item
                            .r#type
                            .clone()
                            .with_role(number.replace(Role::FunctionInput))],
                        output: Box::new(expression.item.r#type.clone().with_role(WithInfo {
                            info: expression.info.clone(),
                            item: Role::FunctionOutput,
                        })),
                    },
                    expression.info.clone(),
                    Vec::new(),
                );

                try_unify_expression(
                    context.driver,
                    unit.as_mut(),
                    &unit_type,
                    context.type_context,
                    context.error_queue,
                );

                ExpressionKind::Call {
                    function: unit.boxed(),
                    inputs: vec![number],
                }
            } else if let TypeKind::Function {
                inputs: input_types,
                output: output_type,
            } = &function.item.r#type.kind
            {
                if input_types.len() == inputs.len() {
                    for (input, input_type) in inputs.iter_mut().zip(input_types) {
                        try_unify_expression(
                            context.driver,
                            input.as_mut(),
                            input_type,
                            context.type_context,
                            context.error_queue,
                        );
                    }

                    try_unify(
                        context.driver,
                        WithInfo {
                            info: expression.info.clone(),
                            item: &expression.item.r#type,
                        },
                        output_type,
                        context.type_context,
                        context.error_queue,
                    );

                    let inputs = inputs
                        .into_iter()
                        .map(|input| resolve_expression(input, context))
                        .collect::<Vec<_>>();

                    let function = resolve_expression(function.unboxed(), context);

                    ExpressionKind::Call {
                        function: function.boxed(),
                        inputs,
                    }
                } else {
                    context.error_queue.borrow_mut().push(WithInfo {
                        info: expression.info.clone(),
                        item: QueuedError::WrongNumberOfInputs {
                            actual: inputs.len() as u32,
                            expected: input_types.len() as u32,
                        },
                    });

                    ExpressionKind::Unknown(None)
                }
            } else {
                let function_type = Type::new(
                    TypeKind::Function {
                        inputs: inputs
                            .iter()
                            .map(|input| {
                                input
                                    .item
                                    .r#type
                                    .clone()
                                    .with_role(input.replace(Role::FunctionInput))
                            })
                            .collect(),
                        output: Box::new(expression.item.r#type.clone().with_role(WithInfo {
                            info: expression.info.clone(),
                            item: Role::FunctionOutput,
                        })),
                    },
                    function.info.clone(),
                    Vec::new(),
                );

                try_unify_expression(
                    context.driver,
                    function.as_deref_mut(),
                    &function_type,
                    context.type_context,
                    context.error_queue,
                );

                let inputs = inputs
                    .into_iter()
                    .map(|input| resolve_expression(input, context))
                    .collect::<Vec<_>>();

                let function = resolve_expression(function.unboxed(), context);

                ExpressionKind::Call {
                    function: function.boxed(),
                    inputs,
                }
            }
        }
        ExpressionKind::When { input, arms } => {
            let input = resolve_expression(input.unboxed(), context).boxed();

            let arms = arms
                .into_iter()
                .map(|arm| {
                    arm.map(|arm| {
                        resolve_pattern(
                            arm.pattern.as_ref(),
                            input.as_ref().map(|input| &input.r#type),
                            InferContext::from_resolve_context(context),
                        );

                        Arm {
                            pattern: arm.pattern,
                            body: resolve_expression(arm.body, context),
                        }
                    })
                })
                .collect::<Vec<_>>();

            ExpressionKind::When { input, arms }
        }
        ExpressionKind::Intrinsic { name, inputs } => ExpressionKind::Intrinsic {
            name,
            inputs: inputs
                .into_iter()
                .map(|expression| resolve_expression(expression, context))
                .collect(),
        },
        ExpressionKind::Initialize { pattern, value } => {
            let value = resolve_expression(value.unboxed(), context).boxed();

            resolve_pattern(
                pattern.as_ref(),
                value.as_ref().map(|value| &value.r#type),
                InferContext::from_resolve_context(context),
            );

            ExpressionKind::Initialize { pattern, value }
        }
        ExpressionKind::Marker(r#type) => ExpressionKind::Marker(r#type),
        ExpressionKind::UnresolvedStructure(fields) => {
            expression
                .item
                .r#type
                .apply_in_context_mut(context.type_context);

            match &expression.item.r#type.kind {
                TypeKind::Variable(_) | TypeKind::Opaque(_) => ExpressionKind::UnresolvedStructure(
                    fields
                        .into_iter()
                        .map(|field_value| {
                            field_value.map(|field_value| StructureFieldValue {
                                name: field_value.name,
                                value: resolve_expression(field_value.value, context),
                            })
                        })
                        .collect(),
                ),
                TypeKind::Declared { path, .. } => {
                    let path = path.clone();

                    let type_declaration = context.driver.get_type_declaration(&path);

                    let mut field_types = match type_declaration.item.representation.item {
                        crate::TypeRepresentation::Structure(fields) => fields,
                        _ => todo!("report error"),
                    };

                    let instantiation_context = InstantiationContext::from_parameters(
                        context.driver,
                        type_declaration.item.parameters.clone(),
                        context.type_context,
                        &expression.info,
                        context.errors,
                    );

                    let fields = fields
                        .into_iter()
                        .filter_map(|field_value| {
                            let field_value_info = field_value.info.clone();

                            field_value.filter_map(|field_value| {
                                let declared_type = match field_types.remove(&field_value.name) {
                                    Some(field) => infer_type(
                                        field.item.r#type.as_ref(),
                                        field.replace(Role::StructureField),
                                        Some(context.type_context),
                                    )
                                    .instantiate(context.driver, &instantiation_context),
                                    None => {
                                        context.error_queue.borrow_mut().push(WithInfo {
                                            info: field_value_info.clone(),
                                            item: QueuedError::ExtraField,
                                        });

                                        return None;
                                    }
                                };

                                let mut value = resolve_expression(field_value.value, context);

                                try_unify_expression(
                                    context.driver,
                                    value.as_mut(),
                                    &declared_type,
                                    context.type_context,
                                    context.error_queue,
                                );

                                Some(StructureFieldValue {
                                    name: field_value.name,
                                    value,
                                })
                            })
                        })
                        .collect();

                    let missing_fields = field_types.into_keys().collect::<Vec<_>>();
                    if !missing_fields.is_empty() {
                        context.error_queue.borrow_mut().push(WithInfo {
                            info: expression.info.clone(),
                            item: QueuedError::MissingFields(missing_fields),
                        });
                    }

                    let instantiated_declared_type = Type::new(
                        TypeKind::Declared {
                            path: path.clone(),
                            parameters: instantiation_context.into_types_for_parameters(),
                        },
                        expression.info.clone(),
                        Vec::new(),
                    );

                    try_unify(
                        context.driver,
                        WithInfo {
                            info: expression.info.clone(),
                            item: &instantiated_declared_type,
                        },
                        &expression.item.r#type,
                        context.type_context,
                        context.error_queue,
                    );

                    ExpressionKind::ResolvedStructure {
                        structure: path,
                        fields,
                    }
                }
                _ => {
                    context.error_queue.borrow_mut().push(WithInfo {
                        info: expression.info.clone(),
                        item: QueuedError::NotAStructure(expression.item.r#type.clone()),
                    });

                    ExpressionKind::Unknown(None)
                }
            }
        }
        ExpressionKind::ResolvedStructure { structure, fields } => {
            ExpressionKind::ResolvedStructure {
                structure,
                fields: fields
                    .into_iter()
                    .map(|field| {
                        field.map(|field| StructureFieldValue {
                            name: field.name,
                            value: resolve_expression(field.value, context),
                        })
                    })
                    .collect(),
            }
        }
        ExpressionKind::Variant { variant, values } => ExpressionKind::Variant {
            variant,
            values: values
                .into_iter()
                .map(|expression| resolve_expression(expression, context))
                .collect(),
        },
        ExpressionKind::Wrapper(value) => {
            ExpressionKind::Wrapper(resolve_expression(value.unboxed(), context).boxed())
        }
        ExpressionKind::Tuple(elements) => ExpressionKind::Tuple(
            elements
                .into_iter()
                .map(|expression| resolve_expression(expression, context))
                .collect(),
        ),
        ExpressionKind::Format { segments, trailing } => ExpressionKind::Format {
            segments: segments
                .into_iter()
                .map(|segment| FormatSegment {
                    text: segment.text,
                    value: resolve_expression(segment.value, context),
                })
                .collect(),
            trailing,
        },
    };

    WithInfo {
        info: expression.info,
        item: Expression {
            r#type: expression.item.r#type,
            kind,
        },
    }
}

fn resolve_item<D: Driver>(
    path: &D::Path,
    mut use_expression: WithInfo<D::Info, &mut Expression<D>>,
    allow_unresolved_bounds: bool,
    context: &ResolveContext<'_, D>,
) -> Result<Option<Vec<Type<D>>>, WithInfo<D::Info, QueuedError<D>>> {
    let item_declaration = context.driver.get_constant_declaration(path);

    let use_info = use_expression.info.clone();

    // Instantiate the items' type, substituting inferred parameters with opaque
    // type variables

    let instantiated_declared_role = use_expression.replace(Role::Annotation);

    let instantiation_context = InstantiationContext::from_parameters_with_options(
        context.driver,
        item_declaration.item.parameters,
        context.type_context,
        &use_info,
        context.errors,
        InstantiationOptions {
            instantiate_inferred_parameters_as_opaque: true,
        },
    );

    let instantiated_declared_type = infer_type(
        WithInfo {
            info: use_info.clone(),
            item: &item_declaration.item.r#type.item,
        },
        instantiated_declared_role,
        Some(context.type_context),
    )
    .instantiate(context.driver, &instantiation_context);

    let instantiated_bounds = item_declaration
        .item
        .bounds
        .into_iter()
        .map(|bound| {
            infer_instance(bound)
                .map(|bound| bound.instantiate(context.driver, &instantiation_context))
        })
        .collect::<Vec<_>>();

    // Unify instantiated declared type with type of referring expression

    try_unify_expression(
        context.driver,
        use_expression.as_deref_mut(),
        &instantiated_declared_type,
        context.type_context,
        context.error_queue,
    );

    // Check bounds

    let evaluate_bounds = {
        let instantiated_declared_type = instantiated_declared_type.clone();
        let use_type = use_expression.item.r#type.clone();
        let info = &use_info;

        move |bounds: &[WithInfo<D::Info, Instance<D>>], allow_unresolved: bool| {
            bounds
                .iter()
                .map(|bound| {
                    let query = WithInfo {
                        info: info.clone(),
                        item: bound.item.clone(),
                    };

                    match resolve_trait(query.as_ref(), context) {
                        Ok(()) => Ok(()),
                        Err(WithInfo {
                            item: QueuedError::UnresolvedInstance { .. },
                            ..
                        }) if allow_unresolved => Ok(()),
                        Err(error) => {
                            // Attempt to get a better error message by unifying the
                            // declared type with the use type again, now that the
                            // bounds have influenced the type
                            if !unify(
                                context.driver,
                                &use_type,
                                &instantiated_declared_type,
                                context.type_context,
                            ) {
                                Err(WithInfo {
                                    info: info.clone(),
                                    item: QueuedError::Mismatch {
                                        actual: use_type.clone(),
                                        expected: instantiated_declared_type.clone(),
                                    },
                                })
                            } else {
                                Err(error)
                            }
                        }
                    }
                })
                .collect::<Vec<Result<(), _>>>()
        }
    };

    for result in evaluate_bounds(&instantiated_bounds, true) {
        result?;
    }

    // Turn the opaque type variables back into regular type variables so they
    // can be inferred now that we've checked the bounds

    let instantiated_declared_type =
        instantiated_declared_type.instantiate_opaque_in_context(context.type_context);

    let instantiated_bounds = instantiated_bounds
        .into_iter()
        .map(|bound| {
            bound
                .as_ref()
                .map(|instance| instance.instantiate_opaque(context.type_context))
        })
        .collect::<Vec<_>>();

    // Now that we've determined the types of the non-inferred parameters from the bounds,
    // determine the types of the inferred parameters by re-evaluating the bounds

    let mut success = true;
    for result in evaluate_bounds(&instantiated_bounds, false) {
        if let Err(error) = result {
            if allow_unresolved_bounds {
                success = false;
            } else {
                return Err(error);
            }
        }
    }

    try_unify_expression(
        context.driver,
        use_expression,
        &instantiated_declared_type,
        context.type_context,
        context.error_queue,
    );

    Ok(success.then(|| {
        instantiation_context
            .into_types_for_parameters()
            .into_iter()
            .map(|parameter| parameter.instantiate_opaque_in_context(context.type_context))
            .collect()
    }))
}

fn resolve_trait_parameters_from_type<D: Driver>(
    path: &D::Path,
    use_expression: WithInfo<D::Info, &mut Expression<D>>,
    context: &ResolveContext<'_, D>,
) -> Vec<Type<D>> {
    let trait_declaration = context.driver.get_trait_declaration(path);

    let use_info = use_expression.info.clone();

    let role = trait_declaration.replace(Role::Trait);

    let instantiation_context = InstantiationContext::from_parameters(
        context.driver,
        trait_declaration.item.parameters,
        context.type_context,
        &use_info,
        context.errors,
    );

    let trait_type = infer_type(
        trait_declaration.item.r#type.as_ref(),
        role,
        Some(context.type_context),
    )
    .instantiate(context.driver, &instantiation_context);

    try_unify_expression(
        context.driver,
        use_expression,
        &trait_type,
        context.type_context,
        context.error_queue,
    );

    instantiation_context.into_types_for_parameters()
}

fn resolve_trait<D: Driver>(
    query: WithInfo<D::Info, &Instance<D>>,
    context: &ResolveContext<'_, D>,
) -> Result<(), WithInfo<D::Info, QueuedError<D>>> {
    type Candidate<D> = (
        TypeContext<D>,
        WithInfo<<D as Driver>::Info, Instance<D>>,
        Vec<WithInfo<<D as Driver>::Info, Instance<D>>>,
    );

    fn pick_from_candidates<D: Driver>(
        mut candidates: Vec<Candidate<D>>,
        query: WithInfo<D::Info, &Instance<D>>,
        stack: &[WithInfo<D::Info, &Instance<D>>],
    ) -> Result<Option<Candidate<D>>, WithInfo<D::Info, QueuedError<D>>> {
        match candidates.len() {
            0 => Ok(None),
            1 => Ok(Some(candidates.pop().unwrap())),
            _ => Err(query.map(|query| QueuedError::UnresolvedInstance {
                instance: query.clone(),
                candidates: candidates
                    .into_iter()
                    .map(|(_, candidate, _)| candidate.info)
                    .collect(),
                stack: stack
                    .iter()
                    .map(|instance| instance.as_deref().map(Instance::clone))
                    .collect(),
            })),
        }
    }

    fn resolve_trait_inner<D: Driver>(
        query: WithInfo<D::Info, &Instance<D>>,
        context: &ResolveContext<'_, D>,
        stack: &[WithInfo<D::Info, &Instance<D>>],
    ) -> Result<WithInfo<D::Info, Instance<D>>, WithInfo<D::Info, QueuedError<D>>> {
        let r#trait = query.item.r#trait.clone();

        let recursion_limit = context.driver.recursion_limit();

        if context.recursion_stack.borrow().len() as u32 > recursion_limit {
            return Err(query.replace(QueuedError::RecursionLimit));
        }

        // First, check if there are any bound instances that match
        for bound_instances in context.bound_instances.borrow().iter() {
            let mut candidates = Vec::new();
            for bound_instance in bound_instances {
                let new_type_context = context.type_context.clone();

                let query = query.as_deref().map(Clone::clone);

                let bound = bound_instance.as_ref().map(Clone::clone);

                if unify_instance_with_options(
                    context.driver,
                    query.as_ref(),
                    bound.as_ref(),
                    &new_type_context,
                    UnifyOptions {
                        require_equal_type_parameters: true,
                        ..Default::default()
                    },
                ) {
                    candidates.push((new_type_context, bound, Vec::new()));
                }
            }

            if let Some((new_type_context, candidate, _)) =
                pick_from_candidates(candidates, query.clone(), stack)?
            {
                context.type_context.replace_with(new_type_context);
                return Ok(candidate);
            }
        }

        // Then, check if there are any declared instances that match
        let mut candidates = Vec::new();
        for path in context.driver.get_instances_for_trait(&r#trait) {
            let instance_declaration = context.driver.get_instance_declaration(&path);

            let new_type_context = context.type_context.clone();

            let query = query.as_deref().map(Clone::clone);

            let instantiation_context = InstantiationContext::from_parameters(
                context.driver,
                instance_declaration.item.parameters,
                &new_type_context,
                &instance_declaration.info,
                context.errors,
            );

            let parameters = instance_declaration
                .item
                .instance
                .item
                .parameters
                .iter()
                .map(|parameter| {
                    infer_type(parameter.as_ref(), None, None)
                        .instantiate(context.driver, &instantiation_context)
                })
                .collect::<Vec<_>>();

            let bounds = instance_declaration
                .item
                .bounds
                .into_iter()
                .map(|bound| {
                    infer_instance(bound)
                        .map(|bound| bound.instantiate(context.driver, &instantiation_context))
                })
                .collect::<Vec<_>>();

            let instance = WithInfo {
                info: instance_declaration.info.clone(),
                item: Instance {
                    r#trait: query.item.r#trait.clone(),
                    parameters,
                },
            };

            if unify_instance(
                context.driver,
                query.as_ref(),
                instance.as_ref(),
                &new_type_context,
            ) {
                candidates.push((new_type_context, instance, bounds));
            }
        }

        // If an instance matches, check its bounds
        if let Some((new_type_context, candidate, bounds)) =
            pick_from_candidates(candidates, query.clone(), stack)?
        {
            context.type_context.replace_with(new_type_context);

            for bound in bounds {
                let mut stack = stack.to_vec();
                stack.push(bound.as_ref());

                context
                    .recursion_stack
                    .borrow_mut()
                    .push(bound.info.clone());

                let result = resolve_trait_inner(query.replace(&bound.item), context, &stack);

                context.recursion_stack.borrow_mut().pop();

                result?;
            }

            return Ok(candidate);
        }

        // If nothing matches, raise an error
        Err(WithInfo {
            info: stack.first().unwrap().info.clone(),
            item: QueuedError::UnresolvedInstance {
                instance: query.item.clone(),
                candidates: Vec::new(),
                stack: stack
                    .iter()
                    .map(|instance| instance.as_deref().map(Instance::clone))
                    .collect(),
            },
        })
    }

    resolve_trait_inner(query.clone(), context, &[query])?;

    Ok(())
}

fn substitute_defaults_in_expression<D: Driver>(
    driver: &D,
    expression: WithInfo<D::Info, &mut Expression<D>>,
    context: &ResolveContext<'_, D>,
) -> bool {
    let substituted_subexpression = match &mut expression.item.kind {
        ExpressionKind::Block(statements) => statements.iter_mut().any(|statement| {
            substitute_defaults_in_expression(driver, statement.as_mut(), context)
        }),
        ExpressionKind::Function { body, .. } => {
            substitute_defaults_in_expression(driver, body.as_deref_mut(), context)
        }
        ExpressionKind::Call { function, inputs } => {
            inputs
                .iter_mut()
                .any(|input| substitute_defaults_in_expression(driver, input.as_mut(), context))
                || substitute_defaults_in_expression(driver, function.as_deref_mut(), context)
        }
        ExpressionKind::When { input, arms } => {
            substitute_defaults_in_expression(driver, input.as_deref_mut(), context)
                || arms.iter_mut().any(|arm| {
                    substitute_defaults_in_expression(driver, arm.item.body.as_mut(), context)
                })
        }
        ExpressionKind::Intrinsic { inputs, .. } => inputs
            .iter_mut()
            .any(|input| substitute_defaults_in_expression(driver, input.as_mut(), context)),
        ExpressionKind::Initialize { value, .. } => {
            substitute_defaults_in_expression(driver, value.as_deref_mut(), context)
        }
        ExpressionKind::UnresolvedStructure(fields) => fields.iter_mut().any(|field| {
            substitute_defaults_in_expression(driver, field.item.value.as_mut(), context)
        }),
        ExpressionKind::ResolvedStructure { fields, .. } => fields.iter_mut().any(|field| {
            substitute_defaults_in_expression(driver, field.item.value.as_mut(), context)
        }),
        ExpressionKind::Variant { values, .. } => values
            .iter_mut()
            .any(|value| substitute_defaults_in_expression(driver, value.as_mut(), context)),
        ExpressionKind::Wrapper(value) => {
            substitute_defaults_in_expression(driver, value.as_deref_mut(), context)
        }
        ExpressionKind::Tuple(elements) => elements
            .iter_mut()
            .any(|element| substitute_defaults_in_expression(driver, element.as_mut(), context)),
        ExpressionKind::Format { segments, .. } => segments.iter_mut().any(|segment| {
            substitute_defaults_in_expression(driver, segment.value.as_mut(), context)
        }),
        ExpressionKind::ResolvedConstant { parameters, .. } => parameters
            .iter_mut()
            .any(|r#type| substitute_defaults(driver, r#type, context.type_context)),
        ExpressionKind::Unknown(_)
        | ExpressionKind::Variable(_, _)
        | ExpressionKind::UnresolvedConstant(_)
        | ExpressionKind::UnresolvedTrait(_)
        | ExpressionKind::ResolvedTrait(_)
        | ExpressionKind::Marker(_)
        | ExpressionKind::Number(_)
        | ExpressionKind::Text(_) => false,
    };

    substituted_subexpression
        || substitute_defaults(driver, &mut expression.item.r#type, context.type_context)
}

// region: Finalize

struct FinalizeContext<'a, D: Driver> {
    driver: &'a D,
    type_context: &'a TypeContext<D>,
    bound_instances: RefCell<Vec<Vec<WithInfo<D::Info, Instance<D>>>>>,
    error_queue: Option<&'a RefCell<Vec<WithInfo<D::Info, QueuedError<D>>>>>,
    errors: Option<&'a RefCell<Vec<WithInfo<D::Info, crate::Diagnostic<D>>>>>,
    unresolved_variables: Option<&'a RefCell<HashSet<TypeVariable<D>>>>,
    contains_unknown: &'a Cell<bool>,
    subexpression_types: Option<&'a RefCell<Vec<crate::Type<D>>>>, // in the order of traversal
}

fn finalize_type<D: Driver>(
    r#type: Type<D>,
    context: &FinalizeContext<'_, D>,
) -> WithInfo<D::Info, crate::Type<D>> {
    fn finalize_type_inner<D: Driver>(
        mut r#type: Type<D>,
        context: &FinalizeContext<'_, D>,
        fully_resolved: &mut bool,
    ) -> WithInfo<D::Info, crate::Type<D>> {
        r#type.apply_in_context_mut(context.type_context);

        WithInfo {
            info: r#type.info,
            item: match r#type.kind {
                TypeKind::Variable(var) | TypeKind::Opaque(var) => {
                    if let Some(unresolved_variables) = &context.unresolved_variables {
                        let mut unresolved_variables = unresolved_variables.borrow_mut();

                        // Prevent displaying duplicate errors when a type variable is involved in
                        // more than one expression
                        if !unresolved_variables.contains(&var) {
                            *fully_resolved = false;
                            unresolved_variables.insert(var);
                        }
                    } else {
                        *fully_resolved = false;
                    }

                    crate::Type::Unknown(UnknownTypeId(Some(var.counter)))
                }
                TypeKind::Parameter(path) => crate::Type::Parameter(path),
                TypeKind::Declared { path, parameters } => crate::Type::Declared {
                    path,
                    parameters: parameters
                        .into_iter()
                        .map(|r#type| finalize_type_inner(r#type, context, fully_resolved))
                        .collect(),
                },
                TypeKind::Function { inputs, output } => crate::Type::Function {
                    inputs: inputs
                        .into_iter()
                        .map(|input| finalize_type_inner(input, context, fully_resolved))
                        .collect(),
                    output: finalize_type_inner(*output, context, fully_resolved).boxed(),
                },
                TypeKind::Tuple(elements) => crate::Type::Tuple(
                    elements
                        .into_iter()
                        .map(|r#type| finalize_type_inner(r#type, context, fully_resolved))
                        .collect(),
                ),
                TypeKind::Block(r#type) => crate::Type::Block(
                    finalize_type_inner(*r#type, context, fully_resolved).boxed(),
                ),
                TypeKind::Unknown => {
                    context.contains_unknown.set(true);
                    crate::Type::Unknown(UnknownTypeId::none())
                }
                TypeKind::Intrinsic => crate::Type::Intrinsic,
            },
        }
    }

    let mut fully_resolved = true;
    let finalized_type = finalize_type_inner(r#type, context, &mut fully_resolved);

    if !fully_resolved {
        if let Some(errors) = &context.errors {
            errors.borrow_mut().push(WithInfo {
                info: finalized_type.info.clone(),
                item: crate::Diagnostic::UnknownType(finalized_type.item.clone()),
            });
        }
    }

    if let Some(types) = context.subexpression_types {
        types.borrow_mut().push(finalized_type.item.clone());
    }

    finalized_type
}

fn finalize_expression<D: Driver>(
    mut expression: WithInfo<D::Info, Expression<D>>,
    context: &FinalizeContext<'_, D>,
) -> WithInfo<D::Info, crate::TypedExpression<D>> {
    let kind = match expression.item.kind {
        ExpressionKind::Unknown(path) => crate::TypedExpressionKind::Unknown(path),
        ExpressionKind::Variable(name, variable) => {
            crate::TypedExpressionKind::Variable(name, variable)
        }
        ExpressionKind::UnresolvedConstant(ref path) => {
            let path = path.clone();

            if let Some((error_queue, errors)) =
                context.error_queue.as_ref().zip(context.errors.as_ref())
            {
                let resolve_context = ResolveContext {
                    driver: context.driver,
                    type_context: context.type_context,
                    error_queue,
                    errors,
                    variables: &Default::default(),
                    recursion_stack: &Default::default(),
                    bound_instances: context.bound_instances.clone(),
                };

                match resolve_item(&path, expression.as_mut(), false, &resolve_context) {
                    Ok(Some(parameters)) => {
                        finalize_type(expression.item.r#type.clone(), context);

                        crate::TypedExpressionKind::Constant {
                            path: path.clone(),
                            parameters: parameters
                                .into_iter()
                                .map(|r#type| finalize_type(r#type, context).item)
                                .collect(),
                        }
                    }
                    Ok(None) => crate::TypedExpressionKind::Unknown(Some(path)),
                    Err(error) => {
                        error_queue.borrow_mut().push(error);
                        crate::TypedExpressionKind::Unknown(Some(path))
                    }
                }
            } else {
                crate::TypedExpressionKind::Unknown(Some(path))
            }
        }
        ExpressionKind::UnresolvedTrait(path) => {
            let error = WithInfo {
                info: expression.info.clone(),
                item: crate::Diagnostic::UnknownType(
                    finalize_type(expression.item.r#type.clone(), context).item,
                ),
            };

            if let Some(errors) = &context.errors {
                errors.borrow_mut().push(error);
            }

            crate::TypedExpressionKind::Unknown(Some(path))
        }
        ExpressionKind::ResolvedConstant { path, parameters } => {
            crate::TypedExpressionKind::Constant {
                path,
                parameters: parameters
                    .iter()
                    .map(|r#type| finalize_type(r#type.clone(), context).item)
                    .collect(),
            }
        }
        ExpressionKind::ResolvedTrait(path) => crate::TypedExpressionKind::Trait(path),
        ExpressionKind::Number(number) => crate::TypedExpressionKind::Number(number),
        ExpressionKind::Text(text) => crate::TypedExpressionKind::Text(text),
        ExpressionKind::Block(statements) => crate::TypedExpressionKind::Block(
            statements
                .into_iter()
                .map(|expression| finalize_expression(expression, context))
                .collect(),
        ),
        ExpressionKind::Function { inputs, body } => crate::TypedExpressionKind::Function {
            inputs,
            body: finalize_expression(body.unboxed(), context).boxed(),
        },
        ExpressionKind::Call { function, inputs } => {
            let inputs = inputs
                .into_iter()
                .map(|input| finalize_expression(input, context))
                .collect::<Vec<_>>();

            let function = finalize_expression(function.unboxed(), context);

            crate::TypedExpressionKind::Call {
                function: function.boxed(),
                inputs,
            }
        }
        ExpressionKind::When { input, arms } => crate::TypedExpressionKind::When {
            input: finalize_expression(input.unboxed(), context).boxed(),
            arms: arms
                .into_iter()
                .map(|arm| {
                    arm.map(|arm| crate::TypedArm {
                        pattern: arm.pattern,
                        body: finalize_expression(arm.body, context),
                    })
                })
                .collect(),
        },
        ExpressionKind::Intrinsic { name, inputs } => crate::TypedExpressionKind::Intrinsic {
            name,
            inputs: inputs
                .into_iter()
                .map(|expression| finalize_expression(expression, context))
                .collect(),
        },
        ExpressionKind::Initialize { pattern, value } => crate::TypedExpressionKind::Initialize {
            pattern,
            value: finalize_expression(value.unboxed(), context).boxed(),
        },
        ExpressionKind::Marker(r#type) => crate::TypedExpressionKind::Marker(r#type),
        ExpressionKind::UnresolvedStructure(_) => {
            let error = WithInfo {
                info: expression.info.clone(),
                item: crate::Diagnostic::UnknownType(
                    finalize_type(expression.item.r#type.clone(), context).item,
                ),
            };

            if let Some(errors) = &context.errors {
                errors.borrow_mut().push(error);
            }

            crate::TypedExpressionKind::Unknown(None)
        }
        ExpressionKind::ResolvedStructure { structure, fields } => {
            crate::TypedExpressionKind::Structure {
                structure,
                fields: fields
                    .into_iter()
                    .map(|field_value| {
                        field_value.map(|field_value| crate::TypedStructureFieldValue {
                            name: field_value.name.clone(),
                            value: finalize_expression(field_value.value, context),
                        })
                    })
                    .collect(),
            }
        }
        ExpressionKind::Wrapper(value) => crate::TypedExpressionKind::Wrapper(
            finalize_expression(value.unboxed(), context).boxed(),
        ),
        ExpressionKind::Variant { variant, values } => crate::TypedExpressionKind::Variant {
            variant,
            values: values
                .into_iter()
                .map(|expression| finalize_expression(expression, context))
                .collect(),
        },
        ExpressionKind::Tuple(elements) => crate::TypedExpressionKind::Tuple(
            elements
                .into_iter()
                .map(|expression| finalize_expression(expression, context))
                .collect(),
        ),
        ExpressionKind::Format { segments, trailing } => crate::TypedExpressionKind::Format {
            segments: segments
                .into_iter()
                .map(|segment| crate::TypedFormatSegment {
                    text: segment.text.clone(),
                    value: finalize_expression(segment.value, context),
                })
                .collect(),
            trailing,
        },
    };

    let r#type = finalize_type(expression.item.r#type.clone(), context).item;

    WithInfo {
        info: expression.info,
        item: crate::TypedExpression { r#type, kind },
    }
}

fn finalize_instance<D: Driver>(
    instance: Instance<D>,
    context: &FinalizeContext<'_, D>,
) -> crate::Instance<D> {
    crate::Instance {
        r#trait: instance.r#trait,
        parameters: instance
            .parameters
            .into_iter()
            .map(|r#type| finalize_type(r#type, context))
            .collect(),
    }
}

#[allow(dead_code)]
fn debug_instance<D: Driver>(instance: &Instance<D>, context: &TypeContext<D>) -> String {
    format!(
        "({:?}{})",
        debug_path(&instance.r#trait),
        instance
            .parameters
            .iter()
            .fold(String::new(), |mut result, parameter| {
                use std::fmt::Write;
                write!(&mut result, " {}", debug_type(parameter, context)).unwrap();
                result
            })
    )
}

#[allow(dead_code)]
fn debug_type<D: Driver>(r#type: &Type<D>, context: &TypeContext<D>) -> String {
    let r#type = r#type.apply_in_context(context);

    match r#type.kind {
        TypeKind::Variable(variable) => format!("{{{:?}}}", variable),
        TypeKind::Opaque(_) => String::from("{opaque}"),
        TypeKind::Parameter(path) => format!("({})", debug_path(&path)),
        TypeKind::Declared { path, parameters } => format!(
            "({:?}{})",
            debug_path(&path),
            parameters
                .into_iter()
                .fold(String::new(), |mut result, parameter| {
                    use std::fmt::Write;
                    write!(&mut result, " {}", debug_type(&parameter, context)).unwrap();
                    result
                })
        ),
        TypeKind::Function { inputs, output } => {
            format!(
                "({}-> {})",
                inputs.into_iter().fold(String::new(), |mut result, input| {
                    use std::fmt::Write;
                    write!(&mut result, "{} ", debug_type(&input, context)).unwrap();
                    result
                }),
                debug_type(&output, context),
            )
        }
        TypeKind::Tuple(elements) => format!(
            "({})",
            elements.iter().fold(String::new(), |mut result, element| {
                use std::fmt::Write;
                write!(&mut result, " {} ,", debug_type(element, context)).unwrap();
                result
            })
        ),
        TypeKind::Block(r#type) => format!("{{{}}}", debug_type(&r#type, context)),
        TypeKind::Unknown => String::from("_"),
        TypeKind::Intrinsic => String::from("intrinsic"),
    }
}

#[allow(dead_code)]
fn debug_path(path: &(impl Debug + serde::Serialize)) -> String {
    serde_json::to_value(path)
        .ok()
        .and_then(|value| value.as_str().map(ToString::to_string))
        .unwrap_or_else(|| format!("{path:?}"))
}
