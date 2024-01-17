use crate::{Driver, Role};
use derivative::Derivative;
use std::{
    cell::{Cell, RefCell},
    collections::{btree_map, BTreeMap, HashMap, HashSet},
    fmt::Debug,
    mem,
    rc::Rc,
};
use wipple_util::WithInfo;

pub struct ItemDeclarationInner<D: Driver> {
    bounds: Vec<WithInfo<D::Info, crate::Instance<D>>>,
    r#type: WithInfo<D::Info, crate::Type<D>>,
    body: WithInfo<D::Info, crate::UntypedExpression<D>>,
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
                    item: crate::Type::Unknown, // the top level can be any type
                },
                body: WithInfo {
                    info: driver.top_level_info(),
                    item: crate::UntypedExpression::Block(code),
                },
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

    let type_context = TypeContext::new();

    let variables: RefCell<HashMap<_, _>> = Default::default();
    let infer_context = InferContext {
        driver,
        type_context: &type_context,
        error_queue: &error_queue,
        errors: &errors,
        variables: &variables,
    };

    let declared_type = infer_type(
        &item_declaration.item.r#type.item,
        item_declaration.replace(Role::Annotation),
        Some(&type_context),
    );

    let mut body = infer_expression(item_declaration.item.body, infer_context);
    try_unify_expression(driver, body.as_mut(), &declared_type, &error_queue);

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

    let item = loop {
        if recursion_stack.borrow().len() as u32 > recursion_limit {
            errors.borrow_mut().push(WithInfo {
                info: queued.body.info.clone(),
                item: crate::Error::RecursionLimit,
            });

            let finalize_context = FinalizeContext { errors: None };

            break finalize_expression(queued.body, &finalize_context);
        }

        let fully_resolved = Cell::new(true);
        let progress = Cell::new(false);
        let resolve_context = ResolveContext {
            driver,
            type_context: &queued.type_context,
            error_queue: &error_queue,
            errors: &errors,
            variables: &variables,
            fully_resolved: &fully_resolved,
            progress: &progress,
            recursion_stack: &recursion_stack,
            bound_instances: RefCell::from(
                queued
                    .bounds
                    .borrow()
                    .iter()
                    .map(|bounds| {
                        bounds
                            .iter()
                            .map(|bound| bound.as_ref().map(Instance::clone_in_current_context))
                            .collect()
                    })
                    .collect::<Vec<Vec<_>>>(),
            ),
        };

        queued.body = resolve_expression(queued.body, &resolve_context);

        if fully_resolved.get() || !progress.get() {
            try_unify_expression(driver, queued.body.as_mut(), &declared_type, &error_queue);

            let finalize_context = FinalizeContext {
                errors: Some(&errors),
            };

            break finalize_expression(queued.body, &finalize_context);
        } else {
            recursion_stack.borrow_mut().push(queued.use_info.clone());
        }
    };

    let mut errors = errors.into_inner();
    report_queued_errors(error_queue.into_inner(), &mut errors);

    crate::Result { item, errors }
}

pub fn instances_overlap<D: Driver>(
    driver: &D,
    instances: impl IntoIterator<Item = D::Path>,
) -> Vec<WithInfo<D::Info, crate::Error<D>>> {
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

            let instantiate_instance =
                |parameters: Vec<<D as Driver>::Path>,
                 instance: WithInfo<D::Info, &mut Instance<D>>| {
                    let type_context = TypeContext::new();
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
                };

            let mut instance = instance
                .as_ref()
                .map(|instance| instance.clone_in_current_context());

            instantiate_instance(parameters.clone(), instance.as_mut());

            let mut other_instance = other_instance
                .as_ref()
                .map(|instance| instance.clone_in_current_context());

            instantiate_instance(other_parameters.clone(), other_instance.as_mut());

            if unify_instance(driver, instance.as_mut(), other_instance.as_ref()) {
                overlapping.insert((index, other_index));

                errors.push(WithInfo {
                    info: info.clone(),
                    item: crate::Error::OverlappingInstances {
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

    let type_context = TypeContext::new();
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
        let mut r#type = instantiation_context.type_for_parameter(driver, &trait_parameter);

        assert!(unify(
            driver,
            &mut r#type,
            &infer_type(instance_parameter, role.clone(), None),
        )
        .is_some());
    }

    let r#type = infer_type(
        &trait_declaration.item.r#type.item,
        role,
        Some(&type_context),
    )
    .instantiate(driver, &instantiation_context);

    assert!(errors.into_inner().is_empty());

    let finalize_context = FinalizeContext { errors: None };

    trait_declaration
        .item
        .r#type
        .replace(finalize_type(r#type, &instance.info, &finalize_context))
}

// Instead of reporting unification errors immediately, queue them and then
// report them all once all type information has been collected.
enum QueuedError<D: Driver> {
    RecursionLimit,

    Mismatch {
        actual: Type<D>,
        expected: Type<D>,
    },

    DisallowedCoercion(Type<D>),

    UnresolvedInstance {
        r#trait: D::Path,
        parameters: Vec<Type<D>>,
        candidates: Vec<D::Info>,
        stack: Vec<D::Info>,
    },

    MissingFields(Vec<String>),

    ExtraField,
}

fn report_queued_errors<D: Driver>(
    error_queue: Vec<WithInfo<D::Info, QueuedError<D>>>,
    errors: &mut Vec<WithInfo<D::Info, crate::Error<D>>>,
) {
    let finalize_context = FinalizeContext { errors: None };

    errors.extend(error_queue.into_iter().map(|error| {
        let info = error.info.clone();

        error.map(|error| match error {
            QueuedError::RecursionLimit => crate::Error::RecursionLimit,
            QueuedError::Mismatch { actual, expected } => crate::Error::Mismatch {
                actual_roles: actual.roles.clone(),
                actual: finalize_type(actual, &info, &finalize_context),
                expected_roles: expected.roles.clone(),
                expected: finalize_type(expected, &info, &finalize_context),
            },
            QueuedError::DisallowedCoercion(r#type) => {
                crate::Error::DisallowedCoercion(finalize_type(r#type, &info, &finalize_context))
            }
            QueuedError::UnresolvedInstance {
                r#trait,
                parameters,
                candidates,
                stack,
            } => crate::Error::UnresolvedInstance {
                r#trait,
                parameters: parameters
                    .into_iter()
                    .map(|r#type| finalize_type(r#type, &info, &finalize_context))
                    .collect(),
                candidates,
                stack,
            },
            QueuedError::MissingFields(fields) => crate::Error::MissingFields(fields),
            QueuedError::ExtraField => crate::Error::ExtraField,
        })
    }));
}

// region: Types and type variables

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
struct Type<D: Driver> {
    roles: Vec<WithInfo<D::Info, Role>>,
    kind: TypeKind<D>,
}

impl<D: Driver> Type<D> {
    fn new(kind: TypeKind<D>, roles: Vec<WithInfo<D::Info, Role>>) -> Self {
        Type { roles, kind }
    }

    fn with_role(self, role: WithInfo<D::Info, Role>) -> Self {
        self.with_roles(std::iter::once(role))
    }

    fn with_roles(mut self, roles: impl IntoIterator<Item = WithInfo<D::Info, Role>>) -> Self {
        self.roles.extend(roles);
        self
    }

    #[must_use]
    fn apply_in_current_context(&self) -> Self {
        let mut r#type = self.clone_in_current_context();
        r#type.apply_in_current_context_mut();
        r#type
    }

    fn apply_in_current_context_mut(&mut self) {
        match &mut self.kind {
            TypeKind::Variable(variable) => {
                if let Some(r#type) =
                    variable.with_substitution_mut(|substitution| match substitution {
                        btree_map::Entry::Vacant(_) => None,
                        btree_map::Entry::Occupied(entry) => {
                            Some(entry.get().clone_in_current_context())
                        }
                    })
                {
                    self.kind = r#type.kind;
                    self.roles.extend(r#type.roles);
                    self.apply_in_current_context_mut();
                }
            }
            TypeKind::Opaque(_) => {}
            TypeKind::Parameter(_) => {}
            TypeKind::Declared { parameters, .. } => {
                for r#type in parameters {
                    r#type.apply_in_current_context_mut();
                }
            }
            TypeKind::Function { input, output } => {
                input.apply_in_current_context_mut();
                output.apply_in_current_context_mut();
            }
            TypeKind::Tuple(elements) => {
                for r#type in elements {
                    r#type.apply_in_current_context_mut();
                }
            }
            TypeKind::Lazy(r#type) => {
                r#type.apply_in_current_context_mut();
            }
            TypeKind::Unknown => {}
        }
    }
}

impl<D: Driver> Type<D> {
    fn clone_in_current_context(&self) -> Self {
        Type {
            roles: self.roles.clone(),
            kind: self.kind.clone_in_current_context(),
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
enum TypeKind<D: Driver> {
    Variable(TypeVariable<D>),
    Opaque(TypeVariable<D>),
    Parameter(D::Path),
    Declared {
        path: D::Path,
        parameters: Vec<Type<D>>,
    },
    Function {
        input: Box<Type<D>>,
        output: Box<Type<D>>,
    },
    Tuple(Vec<Type<D>>),
    Lazy(Box<Type<D>>),
    Unknown,
}

impl<D: Driver> TypeKind<D> {
    fn clone_in_current_context(&self) -> Self {
        match self {
            TypeKind::Variable(variable) => TypeKind::Variable(variable.clone_in_current_context()),
            TypeKind::Opaque(variable) => TypeKind::Opaque(variable.clone_in_current_context()),
            TypeKind::Parameter(path) => TypeKind::Parameter(path.clone()),
            TypeKind::Declared {
                path,
                parameters: params,
            } => TypeKind::Declared {
                path: path.clone(),
                parameters: params.iter().map(Type::clone_in_current_context).collect(),
            },
            TypeKind::Function { input, output } => TypeKind::Function {
                input: Box::new(input.clone_in_current_context()),
                output: Box::new(output.clone_in_current_context()),
            },
            TypeKind::Tuple(elements) => TypeKind::Tuple(
                elements
                    .iter()
                    .map(Type::clone_in_current_context)
                    .collect(),
            ),
            TypeKind::Lazy(r#type) => TypeKind::Lazy(Box::new(r#type.clone_in_current_context())),
            TypeKind::Unknown => TypeKind::Unknown,
        }
    }
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
struct TypeVariable<D: Driver> {
    context: TypeContext<D>,
    counter: u32,
}

impl<D: Driver> Debug for TypeVariable<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "TypeVariable({} in context {:x?})",
            self.counter,
            Rc::as_ptr(&self.context.0)
        )
    }
}

impl<D: Driver> TypeVariable<D> {
    fn clone_in_current_context(&self) -> Self {
        TypeVariable {
            context: self.context.shallow_clone(),
            counter: self.counter,
        }
    }
}

struct Instance<D: Driver> {
    r#trait: D::Path,
    parameters: Vec<Type<D>>,
}

impl<D: Driver> Instance<D> {
    fn clone_in_current_context(&self) -> Self {
        Instance {
            r#trait: self.r#trait.clone(),
            parameters: self
                .parameters
                .iter()
                .map(Type::clone_in_current_context)
                .collect(),
        }
    }
}

// region: Type context

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
struct TypeContext<D: Driver>(Rc<TypeContextInner<D>>);

struct TypeContextInner<D: Driver> {
    next_variable: Cell<u32>,
    substitutions: RefCell<BTreeMap<u32, Type<D>>>,
    defaults: RefCell<BTreeMap<u32, Type<D>>>,
}

impl<D: Driver> TypeContextInner<D> {
    fn clone_in_current_context(&self) -> Self {
        TypeContextInner {
            next_variable: self.next_variable.clone(),
            substitutions: RefCell::new(
                self.substitutions
                    .borrow()
                    .iter()
                    .map(|(variable, r#type)| (*variable, r#type.clone_in_current_context()))
                    .collect(),
            ),
            defaults: RefCell::new(
                self.defaults
                    .borrow()
                    .iter()
                    .map(|(variable, r#type)| (*variable, r#type.clone_in_current_context()))
                    .collect(),
            ),
        }
    }
}

impl<D: Driver> TypeContext<D> {
    pub fn new() -> Self {
        TypeContext(Rc::new(TypeContextInner {
            next_variable: Default::default(),
            substitutions: Default::default(),
            defaults: Default::default(),
        }))
    }

    pub fn ptr_eq(this: &Self, other: &Self) -> bool {
        Rc::ptr_eq(&this.0, &other.0)
    }

    pub fn shallow_clone(&self) -> Self {
        TypeContext(self.0.clone())
    }
}

// region: Instantiation context

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
struct InstantiationContext<'a, D: Driver> {
    info: &'a D::Info,
    errors: &'a RefCell<Vec<WithInfo<D::Info, crate::Error<D>>>>,
    types: Vec<(D::Path, Type<D>)>,
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
        type_context: &TypeContext<D>,
        info: &'a D::Info,
        errors: &'a RefCell<Vec<WithInfo<D::Info, crate::Error<D>>>>,
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
        type_context: &TypeContext<D>,
        info: &'a D::Info,
        errors: &'a RefCell<Vec<WithInfo<D::Info, crate::Error<D>>>>,
        options: InstantiationOptions,
    ) -> Self {
        InstantiationContext {
            types: parameters
                .into_iter()
                .map(|path| {
                    let parameter_declaration = driver.get_type_parameter_declaration(&path);

                    let variable = type_context.variable_with_default(
                        parameter_declaration
                            .as_ref()
                            .item
                            .default
                            .as_ref()
                            .map(|r#type| {
                                infer_type(
                                    &r#type.item,
                                    parameter_declaration.replace(Role::TypeParameter),
                                    Some(type_context),
                                )
                            }),
                    );

                    let kind = if parameter_declaration.item.infer.is_some()
                        && options.instantiate_inferred_parameters_as_opaque
                    {
                        TypeKind::Opaque(variable)
                    } else {
                        TypeKind::Variable(variable)
                    };

                    (
                        path,
                        Type::new(
                            kind,
                            vec![parameter_declaration.replace(Role::TypeParameter)],
                        ),
                    )
                })
                .collect(),
            info,
            errors,
        }
    }

    pub fn type_for_parameter(&self, driver: &D, parameter: &D::Path) -> Type<D> {
        self.types
            .iter()
            .find_map(|(instantiation_path, r#type)| {
                driver
                    .paths_are_equal(parameter, instantiation_path)
                    .then_some(r#type)
            })
            .map(Type::clone_in_current_context)
            .unwrap_or_else(|| {
                self.errors.borrow_mut().push(WithInfo {
                    info: self.info.clone(),
                    item: crate::Error::UndeclaredTypeParameter(parameter.clone()),
                });

                Type::new(TypeKind::Unknown, Vec::new())
            })
    }

    pub fn into_types_for_parameters(self) -> Vec<Type<D>> {
        self.types.into_iter().map(|(_, r#type)| r#type).collect()
    }
}

impl<D: Driver> Type<D> {
    #[must_use]
    fn instantiate(&self, driver: &D, instantiation_context: &InstantiationContext<'_, D>) -> Self {
        let mut r#type = self.clone_in_current_context();
        r#type.instantiate_mut(driver, instantiation_context);
        r#type
    }

    fn instantiate_mut(&mut self, driver: &D, instantiation_context: &InstantiationContext<'_, D>) {
        self.apply_in_current_context_mut();

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
            TypeKind::Function { input, output } => {
                input.instantiate_mut(driver, instantiation_context);
                output.instantiate_mut(driver, instantiation_context);
            }
            TypeKind::Tuple(elements) => {
                for r#type in elements {
                    r#type.instantiate_mut(driver, instantiation_context);
                }
            }
            TypeKind::Lazy(r#type) => {
                r#type.instantiate_mut(driver, instantiation_context);
            }
            TypeKind::Unknown => {}
        }
    }

    #[must_use]
    fn instantiate_opaque(&self) -> Self {
        let mut r#type = self.clone_in_current_context();
        r#type.instantiate_opaque_mut();
        r#type
    }

    fn instantiate_opaque_mut(&mut self) {
        self.apply_in_current_context_mut();

        match &mut self.kind {
            TypeKind::Variable(_) => {}
            TypeKind::Opaque(variable) => {
                self.kind = TypeKind::Variable(variable.clone_in_current_context());
            }
            TypeKind::Parameter(_) => {}
            TypeKind::Declared { parameters, .. } => {
                for r#type in parameters {
                    r#type.instantiate_opaque_mut();
                }
            }
            TypeKind::Function { input, output } => {
                input.instantiate_opaque_mut();
                output.instantiate_opaque_mut();
            }
            TypeKind::Tuple(elements) => {
                for r#type in elements {
                    r#type.instantiate_opaque_mut();
                }
            }
            TypeKind::Lazy(r#type) => {
                r#type.instantiate_opaque_mut();
            }
            TypeKind::Unknown => {}
        }
    }
}

impl<D: Driver> Instance<D> {
    #[must_use]
    fn instantiate(&self, driver: &D, instantiation_context: &InstantiationContext<'_, D>) -> Self {
        let mut bound = self.clone_in_current_context();
        bound.instantiate_mut(driver, instantiation_context);
        bound
    }

    fn instantiate_mut(&mut self, driver: &D, instantiation_context: &InstantiationContext<'_, D>) {
        for parameter in &mut self.parameters {
            parameter.instantiate_mut(driver, instantiation_context);
        }
    }

    #[must_use]
    fn instantiate_opaque(&self) -> Self {
        let mut bound = self.clone_in_current_context();
        bound.instantiate_opaque_mut();
        bound
    }

    fn instantiate_opaque_mut(&mut self) {
        for parameter in &mut self.parameters {
            parameter.instantiate_opaque_mut();
        }
    }
}

// region: Coercions

struct Coercion<D: Driver> {
    lazy: Option<Type<D>>,
}

impl<D: Driver> Default for Coercion<D> {
    fn default() -> Self {
        Coercion { lazy: None }
    }
}

impl<D: Driver> Coercion<D> {
    fn is_none(&self) -> bool {
        self.lazy.is_none()
    }
}

impl<D: Driver> Expression<D> {
    fn apply_coercion(&mut self, info: &D::Info, Coercion { lazy }: Coercion<D>) {
        if let Some(original_type) = lazy {
            *self = Expression {
                r#type: mem::replace(&mut self.r#type, Type::new(TypeKind::Unknown, Vec::new())),
                kind: ExpressionKind::Lazy(WithInfo {
                    info: info.clone(),
                    item: Box::new(Expression {
                        r#type: original_type,
                        kind: mem::replace(&mut self.kind, ExpressionKind::Unknown(None)),
                    }),
                }),
            }
        }
    }
}

// region: Unification

impl<D: Driver> TypeContext<D> {
    pub fn variable(&self) -> TypeVariable<D> {
        self.variable_with_default(None)
    }

    pub fn variable_with_default(&self, default: impl Into<Option<Type<D>>>) -> TypeVariable<D> {
        let counter = self.0.next_variable.get();
        self.0.next_variable.set(counter + 1);

        if let Some(default) = default.into() {
            self.0.defaults.borrow_mut().insert(counter, default);
        }

        TypeVariable {
            context: self.shallow_clone(),
            counter,
        }
    }
}

impl<D: Driver> TypeVariable<D> {
    fn with_substitution_mut<T>(
        &self,
        f: impl FnOnce(btree_map::Entry<'_, u32, Type<D>>) -> T,
    ) -> T {
        f(self
            .context
            .0
            .substitutions
            .borrow_mut()
            .entry(self.counter))
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
    r#type: &mut Type<D>,
    expected_type: &Type<D>,
    options: UnifyOptions,
) -> Option<Coercion<D>> {
    fn unify_variable<D: Driver>(variable: &TypeVariable<D>, r#type: &Type<D>) -> bool {
        if let TypeKind::Variable(other) = &r#type.kind {
            assert!(
                TypeContext::ptr_eq(&variable.context, &other.context),
                "cannot unify type variables from different contexts"
            );

            if variable.counter == other.counter {
                return false;
            }
        }

        variable.with_substitution_mut(|substitution| match substitution {
            btree_map::Entry::Vacant(entry) => {
                entry.insert(r#type.clone_in_current_context());
            }
            btree_map::Entry::Occupied(_) => panic!("variable already has substitution"),
        });

        true
    }

    fn unify_type<D: Driver>(
        driver: &D,
        r#type: &mut Type<D>,
        expected_type: &Type<D>,
        options: UnifyOptions,
    ) -> bool {
        r#type.apply_in_current_context_mut();
        let expected_type = expected_type.apply_in_current_context();

        match (&mut r#type.kind, &expected_type.kind) {
            (TypeKind::Variable(variable), _) => {
                unify_variable(variable, &expected_type);
                r#type.apply_in_current_context_mut();
                true
            }
            (_, TypeKind::Variable(variable)) => {
                unify_variable(variable, r#type);
                r#type.apply_in_current_context_mut();
                true
            }
            (TypeKind::Opaque(_), _) | (_, TypeKind::Opaque(_)) => true,
            (TypeKind::Parameter(parameter), TypeKind::Parameter(expected_parameter)) => {
                driver.paths_are_equal(parameter, expected_parameter)
                    || !options.require_equal_type_parameters
            }
            (TypeKind::Parameter(_), _) | (_, TypeKind::Parameter(_)) => false,
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
                for (r#type, expected_type) in parameters.iter_mut().zip(expected_parameters) {
                    unified &= unify_type(driver, r#type, expected_type, options);
                }

                unified
            }
            (
                TypeKind::Function { input, output },
                TypeKind::Function {
                    input: expected_input,
                    output: expected_output,
                },
            ) => {
                unify_type(driver, input, expected_input, options)
                    & unify_type(driver, output, expected_output, options)
            }
            (TypeKind::Tuple(elements), TypeKind::Tuple(expected_elements)) => {
                if elements.len() != expected_elements.len() {
                    return false;
                }

                let mut unified = true;
                for (r#type, expected_type) in elements.iter_mut().zip(expected_elements) {
                    unified &= unify_type(driver, r#type, expected_type, options);
                }

                unified
            }
            (TypeKind::Lazy(r#type), TypeKind::Lazy(expected_type)) => {
                unify_type(driver, r#type, expected_type, options)
            }
            (_, TypeKind::Lazy(expected_type)) => {
                unify_type(driver, r#type, expected_type, options)
            }
            (TypeKind::Unknown, _) | (_, TypeKind::Unknown) => true,
            _ => false,
        }
    }

    fn get_coercion<D: Driver>(r#type: &Type<D>, expected_type: &Type<D>) -> Coercion<D> {
        let mut coercion = Coercion::default();

        match (&r#type.kind, &expected_type.kind) {
            (kind, TypeKind::Lazy(_)) if !matches!(kind, TypeKind::Lazy(_)) => {
                coercion.lazy = Some(r#type.clone_in_current_context());
            }
            _ => {}
        }

        coercion
    }

    unify_type(driver, r#type, expected_type, options).then(|| get_coercion(r#type, expected_type))
}

#[must_use]
fn unify<D: Driver>(
    driver: &D,
    r#type: &mut Type<D>,
    expected_type: &Type<D>,
) -> Option<Coercion<D>> {
    unify_with_options(driver, r#type, expected_type, UnifyOptions::default())
}

#[must_use]
fn unify_parameters_with_options<D: Driver>(
    driver: &D,
    parameters: &mut [Type<D>],
    expected_parameters: &[Type<D>],
    options: UnifyOptions,
) -> bool {
    let mut unified = true;
    for (r#type, expected_type) in parameters.iter_mut().zip(expected_parameters) {
        unified &= unify_with_options(driver, r#type, expected_type, options).is_some();
    }

    unified
}

#[must_use]
fn unify_instance_with_options<D: Driver>(
    driver: &D,
    instance: WithInfo<D::Info, &mut Instance<D>>,
    expected_instance: WithInfo<D::Info, &Instance<D>>,
    options: UnifyOptions,
) -> bool {
    driver.paths_are_equal(&instance.item.r#trait, &expected_instance.item.r#trait)
        && unify_parameters_with_options(
            driver,
            &mut instance.item.parameters,
            &expected_instance.item.parameters,
            options,
        )
}

#[must_use]
fn unify_instance<D: Driver>(
    driver: &D,
    actual_instance: WithInfo<D::Info, &mut Instance<D>>,
    expected_instance: WithInfo<D::Info, &Instance<D>>,
) -> bool {
    unify_instance_with_options(
        driver,
        actual_instance,
        expected_instance,
        UnifyOptions::default(),
    )
}

fn try_unify_expression<D: Driver>(
    driver: &D,
    expression: WithInfo<D::Info, &mut Expression<D>>,
    expected_type: &Type<D>,
    error_queue: &RefCell<Vec<WithInfo<D::Info, QueuedError<D>>>>,
) {
    if let Some(coercion) = unify(driver, &mut expression.item.r#type, expected_type) {
        expression.item.apply_coercion(&expression.info, coercion);
    } else {
        error_queue.borrow_mut().push(WithInfo {
            info: expression.info.clone(),
            item: QueuedError::Mismatch {
                actual: expression.item.r#type.clone_in_current_context(),
                expected: expected_type.clone_in_current_context(),
            },
        });
    }
}

fn try_unify_without_coercion<D: Driver>(
    driver: &D,
    r#type: WithInfo<D::Info, &mut Type<D>>,
    expected_type: &Type<D>,
    error_queue: &RefCell<Vec<WithInfo<D::Info, QueuedError<D>>>>,
) {
    if let Some(coercion) = unify(driver, r#type.item, expected_type) {
        assert!(coercion.is_none());
    } else {
        error_queue.borrow_mut().push(WithInfo {
            info: r#type.info.clone(),
            item: QueuedError::Mismatch {
                actual: r#type.item.clone_in_current_context(),
                expected: expected_type.clone_in_current_context(),
            },
        });
    }
}

// region: Merging and reification

pub struct TypeContextChange<D: Driver> {
    context: TypeContext<D>,
    delta: u32,
}

impl<D: Driver> Clone for TypeContextChange<D> {
    fn clone(&self) -> Self {
        TypeContextChange {
            context: self.context.shallow_clone(),
            delta: self.delta,
        }
    }
}

impl<D: Driver> TypeContextChange<D> {
    fn from_deep_cloned(context: &TypeContext<D>) -> Self {
        TypeContextChange {
            context: context.shallow_clone(),
            delta: 0,
        }
    }
}

impl<D: Driver> TypeContext<D> {
    #[must_use]
    pub fn deep_clone(&self) -> (Self, TypeContextChange<D>) {
        let new_context = TypeContext(Rc::new(self.0.as_ref().clone_in_current_context()));
        let change = TypeContextChange::from_deep_cloned(&new_context);
        (new_context, change)
    }

    #[must_use]
    pub fn merge(&self, other: &Self) -> TypeContextChange<D> {
        let inner = other.0.as_ref().clone_in_current_context();

        let delta = inner
            .next_variable
            .get()
            .checked_sub(self.0.next_variable.get())
            .expect("cannot merge a context older than `self`");

        TypeContextChange {
            context: TypeContext(Rc::new(inner)),
            delta,
        }
    }
}

impl<D: Driver> Type<D> {
    pub fn clone_in_context(&self, change: &TypeContextChange<D>) -> Self {
        Type {
            roles: self.roles.clone(),
            kind: match &self.kind {
                TypeKind::Variable(variable) => {
                    TypeKind::Variable(variable.clone_in_context(change))
                }
                TypeKind::Opaque(variable) => TypeKind::Opaque(variable.clone_in_context(change)),
                TypeKind::Parameter(path) => TypeKind::Parameter(path.clone()),
                TypeKind::Declared { path, parameters } => TypeKind::Declared {
                    path: path.clone(),
                    parameters: parameters
                        .iter()
                        .map(|parameter| parameter.clone_in_context(change))
                        .collect(),
                },
                TypeKind::Function { input, output } => TypeKind::Function {
                    input: Box::new(input.clone_in_context(change)),
                    output: Box::new(output.clone_in_context(change)),
                },
                TypeKind::Tuple(elements) => TypeKind::Tuple(
                    elements
                        .iter()
                        .map(|element| element.clone_in_context(change))
                        .collect(),
                ),
                TypeKind::Lazy(r#type) => TypeKind::Lazy(Box::new(r#type.clone_in_context(change))),
                TypeKind::Unknown => TypeKind::Unknown,
            },
        }
    }

    pub fn set_context(&mut self, change: &TypeContextChange<D>) {
        match &mut self.kind {
            TypeKind::Variable(variable) => variable.set_context(change),
            TypeKind::Opaque(variable) => variable.set_context(change),
            TypeKind::Parameter(_) => {}
            TypeKind::Declared { parameters, .. } => {
                for parameter in parameters {
                    parameter.set_context(change);
                }
            }
            TypeKind::Function { input, output } => {
                input.set_context(change);
                output.set_context(change);
            }
            TypeKind::Tuple(elements) => {
                for element in elements {
                    element.set_context(change);
                }
            }
            TypeKind::Lazy(r#type) => {
                r#type.set_context(change);
            }
            TypeKind::Unknown => {}
        }
    }
}

impl<D: Driver> TypeVariable<D> {
    fn clone_in_context(&self, change: &TypeContextChange<D>) -> Self {
        let mut variable = self.clone_in_current_context();
        variable.set_context(change);
        variable
    }

    fn set_context(&mut self, change: &TypeContextChange<D>) {
        self.context = change.context.shallow_clone();
        self.counter += change.delta;
    }
}

impl<D: Driver> Instance<D> {
    fn clone_in_context(&self, change: &TypeContextChange<D>) -> Self {
        Instance {
            r#trait: self.r#trait.clone(),
            parameters: self
                .parameters
                .iter()
                .map(|parameter| parameter.clone_in_context(change))
                .collect(),
        }
    }
}

// region: Resolution

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
struct Expression<D: Driver> {
    r#type: Type<D>,
    kind: ExpressionKind<D>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
enum ExpressionKind<D: Driver> {
    Unknown(Option<D::Path>),
    Marker(D::Path),
    Variable(D::Path),
    UnresolvedConstant(D::Path),
    UnresolvedTrait(D::Path),
    ResolvedConstant(D::Path),
    ResolvedTrait(D::Path),
    Number(String),
    Text(String),
    Block(Vec<WithInfo<D::Info, Expression<D>>>),
    Function {
        pattern: WithInfo<D::Info, crate::Pattern<D>>,
        body: WithInfo<D::Info, Box<Expression<D>>>,
    },
    Call {
        function: WithInfo<D::Info, Box<Expression<D>>>,
        input: WithInfo<D::Info, Box<Expression<D>>>,
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
    UnresolvedStructure(Vec<WithInfo<D::Info, StructureFieldValue<D>>>),
    ResolvedStructure {
        structure: D::Path,
        fields: Vec<WithInfo<D::Info, StructureFieldValue<D>>>,
    },
    Variant {
        variant: WithInfo<D::Info, D::Path>,
        values: Vec<WithInfo<D::Info, Expression<D>>>,
    },
    Tuple(Vec<WithInfo<D::Info, Expression<D>>>),
    Format {
        segments: Vec<FormatSegment<D>>,
        trailing: String,
    },
    Semantics {
        name: String,
        body: WithInfo<D::Info, Box<Expression<D>>>,
    },
    Lazy(WithInfo<D::Info, Box<Expression<D>>>),
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
struct FormatSegment<D: Driver> {
    text: String,
    value: WithInfo<D::Info, Expression<D>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
struct StructureFieldValue<D: Driver> {
    name: String,
    value: WithInfo<D::Info, Expression<D>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
struct Arm<D: Driver> {
    pattern: WithInfo<D::Info, crate::Pattern<D>>,
    condition: Option<WithInfo<D::Info, Expression<D>>>,
    body: WithInfo<D::Info, Expression<D>>,
}

// region: Infer

struct InferContext<'a, D: Driver> {
    driver: &'a D,
    type_context: &'a TypeContext<D>,
    error_queue: &'a RefCell<Vec<WithInfo<D::Info, QueuedError<D>>>>,
    errors: &'a RefCell<Vec<WithInfo<D::Info, crate::Error<D>>>>,
    variables: &'a RefCell<HashMap<D::Path, Type<D>>>,
}

impl<'a, D: Driver> InferContext<'a, D> {
    fn from_resolve_context(context: &ResolveContext<'a, D>) -> Self {
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
    r#type: &crate::Type<D>,
    role: impl Into<Option<WithInfo<D::Info, Role>>>,
    type_context: Option<&TypeContext<D>>,
) -> Type<D> {
    Type::new(
        match r#type {
            crate::Type::Parameter(path) => TypeKind::Parameter(path.clone()),
            crate::Type::Declared { path, parameters } => TypeKind::Declared {
                path: path.clone(),
                parameters: parameters
                    .iter()
                    .map(|r#type| infer_type(r#type, None, type_context))
                    .collect(),
            },
            crate::Type::Function { input, output } => TypeKind::Function {
                input: Box::new(infer_type(input, None, type_context)),
                output: Box::new(infer_type(output, None, type_context)),
            },
            crate::Type::Tuple(elements) => TypeKind::Tuple(
                elements
                    .iter()
                    .map(|r#type| infer_type(r#type, None, type_context))
                    .collect(),
            ),
            crate::Type::Lazy(r#type) => {
                TypeKind::Lazy(Box::new(infer_type(r#type, None, type_context)))
            }
            crate::Type::Unknown => match type_context {
                Some(type_context) => TypeKind::Variable(type_context.variable()),
                None => TypeKind::Unknown,
            },
        },
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
            .map(|r#type| infer_type(&r#type, None, None))
            .collect(),
    })
}

fn infer_expression<D: Driver>(
    expression: WithInfo<D::Info, crate::UntypedExpression<D>>,
    context: InferContext<'_, D>,
) -> WithInfo<<D as Driver>::Info, Expression<D>> {
    let info = expression.info.clone();

    expression.map(|expression| match expression {
        crate::UntypedExpression::Unknown => Expression {
            r#type: Type::new(TypeKind::Unknown, Vec::new()),
            kind: ExpressionKind::Unknown(None),
        },
        crate::UntypedExpression::Annotate { value, r#type } => {
            let mut value = infer_expression(value.unboxed(), context);

            let r#type = infer_type(
                &r#type,
                value.replace(Role::Annotation),
                Some(context.type_context),
            );

            try_unify_expression(context.driver, value.as_mut(), &r#type, context.error_queue);

            value.item
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
                Vec::new(),
            );

            Expression {
                r#type,
                kind: ExpressionKind::Marker(path),
            }
        }
        crate::UntypedExpression::Variable(variable) => {
            let r#type = context
                .variables
                .borrow()
                .get(&variable)
                .unwrap_or_else(|| panic!("variable {variable:?} not in context"))
                .clone_in_current_context();

            Expression {
                r#type,
                kind: ExpressionKind::Variable(variable),
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
                &constant_declaration.item.r#type.item,
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
                &trait_declaration.item.r#type.item,
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
                    Vec::new(),
                )
            });

            Expression {
                r#type,
                kind: ExpressionKind::Text(text),
            }
        }
        crate::UntypedExpression::Block(statements) => {
            let statements = statements
                .into_iter()
                .map(|expression| infer_expression(expression, context))
                .collect::<Vec<_>>();

            let r#type = statements.last().map_or_else(
                || {
                    Type::new(
                        TypeKind::Tuple(Vec::new()),
                        vec![WithInfo {
                            info: info.clone(),
                            item: Role::EmptyBlock,
                        }],
                    )
                },
                |expression| expression.item.r#type.clone_in_current_context(),
            );

            Expression {
                r#type,
                kind: ExpressionKind::Block(statements),
            }
        }
        crate::UntypedExpression::Function { pattern, body } => {
            let input_type = Type::new(
                TypeKind::Variable(context.type_context.variable()),
                Vec::new(),
            );

            resolve_pattern(pattern.as_ref(), pattern.replace(&input_type), context);

            let body = infer_expression(body.unboxed(), context);

            Expression {
                r#type: Type::new(
                    TypeKind::Function {
                        input: Box::new(input_type),
                        output: Box::new(
                            body.item
                                .r#type
                                .clone_in_current_context()
                                .with_role(body.replace(Role::FunctionOutput)),
                        ),
                    },
                    Vec::new(),
                ),
                kind: ExpressionKind::Function {
                    pattern,
                    body: body.boxed(),
                },
            }
        }
        crate::UntypedExpression::Call { function, input } => {
            let function = infer_expression(function.unboxed(), context);

            let mut input = infer_expression(input.unboxed(), context);
            let role = input.replace(Role::FunctionInput);
            input.item.r#type = input.item.r#type.with_role(role);

            let r#type = match &function.item.r#type.kind {
                TypeKind::Function { output, .. } => output.as_ref().clone_in_current_context(),
                _ => Type::new(
                    TypeKind::Variable(context.type_context.variable()),
                    vec![function.replace(Role::FunctionOutput)],
                ),
            };

            Expression {
                r#type,
                kind: ExpressionKind::Call {
                    function: function.boxed(),
                    input: input.boxed(),
                },
            }
        }
        crate::UntypedExpression::When { input, arms } => {
            let input = infer_expression(input.unboxed(), context);

            let r#type = Type::new(
                TypeKind::Variable(context.type_context.variable()),
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
                            condition: arm.condition.map(|guard| infer_expression(guard, context)),
                            body: infer_expression(arm.body, context),
                        };

                        if let Some(guard) = &mut arm.condition {
                            let boolean_type = instantiated_language_type(
                                "boolean",
                                &info,
                                context.driver,
                                context.type_context,
                                context.errors,
                            )
                            .unwrap_or_else(|| {
                                Type::new(
                                    TypeKind::Variable(context.type_context.variable()),
                                    Vec::new(),
                                )
                            });

                            try_unify_expression(
                                context.driver,
                                guard.as_mut(),
                                &boolean_type,
                                context.error_queue,
                            );
                        }

                        try_unify_expression(
                            context.driver,
                            arm.body.as_mut(),
                            &r#type,
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
                r#type: Type::new(TypeKind::Tuple(Vec::new()), Vec::new()),
                kind: ExpressionKind::Initialize {
                    pattern,
                    value: value.boxed(),
                },
            }
        }
        crate::UntypedExpression::Structure(fields) => Expression {
            r#type: Type::new(
                TypeKind::Variable(context.type_context.variable()),
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
                                &declared_type.item,
                                declared_variant.replace(Role::VariantElement),
                                Some(context.type_context),
                            )
                            .instantiate(context.driver, &instantiation_context);

                            let mut value = infer_expression(value, context);

                            try_unify_expression(
                                context.driver,
                                value.as_mut(),
                                &declared_type,
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
                Vec::new(),
            );

            Expression {
                r#type,
                kind: ExpressionKind::Variant { variant, values },
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
                            .map(|element| element.item.r#type.clone_in_current_context())
                            .collect(),
                    ),
                    Vec::new(),
                ),
                kind: ExpressionKind::Tuple(elements),
            }
        }
        crate::UntypedExpression::Collection(elements) => {
            let list_type = instantiated_language_type(
                "list",
                &info,
                context.driver,
                context.type_context,
                context.errors,
            );

            let element_type = Type::new(
                TypeKind::Variable(context.type_context.variable_with_default(list_type)),
                vec![WithInfo {
                    info: info.clone(),
                    item: Role::CollectionElement,
                }],
            );

            let default_trait = instantiated_language_trait("default", &info, context);

            elements
                .into_iter()
                .fold(default_trait, |current, expression| {
                    let mut expression = infer_expression(expression, context);

                    try_unify_expression(
                        context.driver,
                        expression.as_mut(),
                        &element_type,
                        context.error_queue,
                    );

                    expression.item.r#type = element_type.clone_in_current_context();

                    let build_collection_trait =
                        instantiated_language_trait("build-collection", &info, context);

                    WithInfo {
                        info: expression.info.clone(),
                        item: Expression {
                            r#type: Type::new(
                                TypeKind::Variable(context.type_context.variable()),
                                Vec::new(),
                            ),
                            kind: ExpressionKind::Call {
                                function: WithInfo {
                                    info: current.info.clone(),
                                    item: Box::new(Expression {
                                        r#type: Type::new(
                                            TypeKind::Variable(context.type_context.variable()),
                                            Vec::new(),
                                        ),
                                        kind: ExpressionKind::Call {
                                            function: build_collection_trait.boxed(),
                                            input: expression.boxed(),
                                        },
                                    }),
                                },
                                input: current.boxed(),
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
                                    Vec::new(),
                                ),
                                kind: ExpressionKind::Call {
                                    function: show_trait.boxed(),
                                    input: value.boxed(),
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
        crate::UntypedExpression::Semantics { name, body } => {
            let body = infer_expression(body.unboxed(), context);

            Expression {
                r#type: body.item.r#type.clone_in_current_context(),
                kind: ExpressionKind::Semantics {
                    name,
                    body: body.boxed(),
                },
            }
        }
    })
}

fn resolve_pattern<D: Driver>(
    pattern: WithInfo<D::Info, &crate::Pattern<D>>,
    r#type: WithInfo<D::Info, &Type<D>>,
    context: InferContext<'_, D>,
) {
    let r#type = r#type.map(|r#type| r#type.clone_in_current_context());

    match &pattern.item {
        crate::Pattern::Unknown => {}
        crate::Pattern::Wildcard => {}
        crate::Pattern::Number(_) => {
            let mut number_type = instantiated_language_type(
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
                        Vec::new(),
                    )
                },
                |r#type| r#type,
            );

            try_unify_without_coercion(
                context.driver,
                r#type.replace(&mut number_type),
                &r#type.item,
                context.error_queue,
            );
        }
        crate::Pattern::Text(_) => {
            let mut text_type = instantiated_language_type(
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
                        Vec::new(),
                    )
                },
                |r#type| r#type,
            );

            try_unify_without_coercion(
                context.driver,
                r#type.replace(&mut text_type),
                &r#type.item,
                context.error_queue,
            );
        }
        crate::Pattern::Variable(variable) => {
            context
                .variables
                .borrow_mut()
                .insert(variable.clone(), r#type.item.clone_in_current_context());
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
                            &mut instantiation_context.type_for_parameter(context.driver, path),
                            r#type,
                        )
                        .is_some());
                    }

                    match type_declaration.item.representation.item {
                        crate::TypeRepresentation::Structure(field_types) => fields
                            .iter()
                            .map(|field| {
                                infer_type(
                                    &field_types.get(&field.item.name).unwrap().item.r#type.item,
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
                    .map(|_| {
                        Type::new(
                            TypeKind::Variable(context.type_context.variable()),
                            Vec::new(),
                        )
                    })
                    .collect::<Vec<_>>(),
            };

            for (field, r#type) in fields.iter().zip(field_types) {
                resolve_pattern(field.item.pattern.as_ref(), field.replace(&r#type), context)
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
                            &mut instantiation_context.type_for_parameter(context.driver, path),
                            r#type,
                        )
                        .is_some());
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
                                        &r#type.item,
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
                                        &r#type.item,
                                        r#type.replace(Role::VariantElement),
                                        Some(context.type_context),
                                    )
                                    .instantiate(context.driver, &instantiation_context)
                                })
                                .collect::<Vec<_>>()
                        }
                        _ => panic!("expected enumeration type"),
                    };

                    let mut enumeration_type = Type::new(
                        TypeKind::Declared {
                            path: enumeration,
                            parameters: instantiation_context.into_types_for_parameters(),
                        },
                        Vec::new(),
                    );

                    try_unify_without_coercion(
                        context.driver,
                        r#type.replace(&mut enumeration_type),
                        &r#type.item,
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
                TypeKind::Tuple(element_types) => element_types
                    .iter()
                    .map(Type::clone_in_current_context)
                    .collect::<Vec<_>>(),
                _ => elements
                    .iter()
                    .map(|_| {
                        Type::new(
                            TypeKind::Variable(context.type_context.variable()),
                            Vec::new(),
                        )
                    })
                    .collect::<Vec<_>>(),
            };

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
    errors: &RefCell<Vec<WithInfo<D::Info, crate::Error<D>>>>,
) -> Option<Type<D>> {
    match driver.path_for_language_type(language_item) {
        Some(path) => {
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
                vec![role],
            ))
        }
        None => {
            errors.borrow_mut().push(WithInfo {
                info: info.clone(),
                item: crate::Error::MissingLanguageItem(language_item),
            });

            None
        }
    }
}

fn instantiated_language_trait<D: Driver>(
    language_item: &'static str,
    info: &D::Info,
    context: InferContext<'_, D>,
) -> WithInfo<D::Info, Expression<D>> {
    match context.driver.path_for_language_trait(language_item) {
        Some(path) => infer_expression(
            WithInfo {
                info: info.clone(),
                item: crate::UntypedExpression::Trait(path),
            },
            context,
        ),
        None => {
            context.errors.borrow_mut().push(WithInfo {
                info: info.clone(),
                item: crate::Error::MissingLanguageItem(language_item),
            });

            WithInfo {
                info: info.clone(),
                item: Expression {
                    r#type: Type::new(TypeKind::Unknown, Vec::new()),
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
    errors: &'a RefCell<Vec<WithInfo<D::Info, crate::Error<D>>>>,
    variables: &'a RefCell<HashMap<D::Path, Type<D>>>,
    fully_resolved: &'a Cell<bool>,
    progress: &'a Cell<bool>,
    recursion_stack: &'a RefCell<Vec<D::Info>>,
    bound_instances: RefCell<Vec<Vec<WithInfo<D::Info, Instance<D>>>>>,
}

impl<D: Driver> ResolveContext<'_, D> {
    fn make_progress(&self) {
        self.progress.set(true);
    }

    fn flag_not_fully_resolved(&self) {
        self.fully_resolved.set(false);
    }
}

fn resolve_expression<D: Driver>(
    mut expression: WithInfo<D::Info, Expression<D>>,
    context: &ResolveContext<'_, D>,
) -> WithInfo<D::Info, Expression<D>> {
    let kind = match expression.item.kind {
        ExpressionKind::Unknown(path) => ExpressionKind::Unknown(path),
        ExpressionKind::Marker(path) => ExpressionKind::Marker(path),
        ExpressionKind::Variable(variable) => ExpressionKind::Variable(variable),
        ExpressionKind::UnresolvedConstant(ref path) => {
            let path = path.clone();

            match resolve_item(&path, expression.as_mut(), context) {
                Ok(true) => {
                    context.make_progress();
                    ExpressionKind::ResolvedConstant(path)
                }
                Ok(false) => {
                    context.flag_not_fully_resolved();
                    ExpressionKind::UnresolvedConstant(path) // try again with more type information
                }
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
                Ok(resolved_trait) => {
                    context.make_progress();

                    expression
                        .item
                        .r#type
                        .set_context(&resolved_trait.item.type_context_change);

                    ExpressionKind::ResolvedTrait(query.item.r#trait)
                }
                Err(error) => {
                    context.error_queue.borrow_mut().push(error);
                    ExpressionKind::Unknown(Some(query.item.r#trait))
                }
            }
        }
        ExpressionKind::ResolvedConstant(path) => ExpressionKind::ResolvedConstant(path),
        ExpressionKind::ResolvedTrait(path) => ExpressionKind::ResolvedTrait(path),
        ExpressionKind::Number(number) => ExpressionKind::Number(number),
        ExpressionKind::Text(text) => ExpressionKind::Text(text),
        ExpressionKind::Block(statements) => ExpressionKind::Block(
            statements
                .into_iter()
                .map(|expression| resolve_expression(expression, context))
                .collect(),
        ),
        ExpressionKind::Function { pattern, body } => {
            if let TypeKind::Function {
                input: input_type, ..
            } = &expression.item.r#type.kind
            {
                resolve_pattern(
                    pattern.as_ref(),
                    pattern.replace(input_type),
                    InferContext::from_resolve_context(context),
                );
            }

            ExpressionKind::Function {
                pattern,
                body: resolve_expression(body.unboxed(), context).boxed(),
            }
        }
        ExpressionKind::Call {
            mut function,
            mut input,
        } => {
            function.item.r#type.apply_in_current_context_mut();

            // If we encounter a unit after a number, treat the unit as a
            // function
            let function_is_number = (|| {
                let number_type = match instantiated_language_type(
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

            if function_is_number {
                let unit = resolve_expression(input.unboxed(), context);
                let number = resolve_expression(function.unboxed(), context);

                ExpressionKind::Call {
                    function: unit.boxed(),
                    input: number.boxed(),
                }
            } else if let TypeKind::Function {
                input: input_type,
                output: output_type,
            } = &function.item.r#type.kind
            {
                try_unify_expression(
                    context.driver,
                    input.as_deref_mut(),
                    input_type,
                    context.error_queue,
                );

                // NOTE: Normally we would need to unify in the other direction
                // because functions' return types are contravariant. But we
                // disallow `lazy` and other coercions in return type position,
                // so this is OK.
                match unify(context.driver, &mut expression.item.r#type, output_type) {
                    Some(coercion) => {
                        // In the case where we still have a coercion (eg. if
                        // a type parameter is returned and that type parameter
                        // is `lazy`, for example), raise an error here.
                        if !coercion.is_none() {
                            context.error_queue.borrow_mut().push(WithInfo {
                                info: expression.info.clone(),
                                item: QueuedError::DisallowedCoercion(
                                    expression.item.r#type.clone_in_current_context(),
                                ),
                            });
                        }
                    }
                    None => {
                        context.error_queue.borrow_mut().push(WithInfo {
                            info: expression.info.clone(),
                            item: QueuedError::Mismatch {
                                actual: expression.item.r#type.clone_in_current_context(),
                                expected: output_type.clone_in_current_context(),
                            },
                        });
                    }
                }

                let input = resolve_expression(input.unboxed(), context);
                let function = resolve_expression(function.unboxed(), context);

                ExpressionKind::Call {
                    function: function.boxed(),
                    input: input.boxed(),
                }
            } else {
                let function_type =
                    Type::new(
                        TypeKind::Function {
                            input: Box::new(
                                input
                                    .item
                                    .r#type
                                    .clone_in_current_context()
                                    .with_role(input.replace(Role::FunctionInput)),
                            ),
                            output: Box::new(
                                expression.item.r#type.clone_in_current_context().with_role(
                                    WithInfo {
                                        info: expression.info.clone(),
                                        item: Role::FunctionOutput,
                                    },
                                ),
                            ),
                        },
                        Vec::new(),
                    );

                try_unify_expression(
                    context.driver,
                    function.as_deref_mut(),
                    &function_type,
                    context.error_queue,
                );

                // Don't resolve `function` and `input` right away; allow them
                // to be queued again so we can collect more type information
                // now that we've unified `function` with a function type
                context.make_progress();

                ExpressionKind::Call { function, input }
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
                            condition: arm
                                .condition
                                .map(|guard| resolve_expression(guard, context)),
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
        ExpressionKind::UnresolvedStructure(fields) => {
            expression.item.r#type.apply_in_current_context_mut();

            match &expression.item.r#type.kind {
                TypeKind::Variable(_) | TypeKind::Opaque(_) => {
                    context.flag_not_fully_resolved();

                    ExpressionKind::UnresolvedStructure(
                        fields
                            .into_iter()
                            .map(|field_value| {
                                field_value.map(|field_value| StructureFieldValue {
                                    name: field_value.name,
                                    value: resolve_expression(field_value.value, context),
                                })
                            })
                            .collect(),
                    )
                }
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
                                        &field.item.r#type.item,
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

                    let mut instantiated_declared_type = Type::new(
                        TypeKind::Declared {
                            path: path.clone(),
                            parameters: instantiation_context.into_types_for_parameters(),
                        },
                        Vec::new(),
                    );

                    match unify(
                        context.driver,
                        &mut instantiated_declared_type,
                        &expression.item.r#type,
                    ) {
                        Some(coercion) => {
                            // In the case where we still have a coercion (eg. if
                            // a type parameter is returned and that type parameter
                            // is `lazy`, for example), raise an error here.
                            if !coercion.is_none() {
                                context.error_queue.borrow_mut().push(WithInfo {
                                    info: expression.info.clone(),
                                    item: QueuedError::DisallowedCoercion(
                                        expression.item.r#type.clone_in_current_context(),
                                    ),
                                });
                            }
                        }
                        None => {
                            context.error_queue.borrow_mut().push(WithInfo {
                                info: expression.info.clone(),
                                item: QueuedError::Mismatch {
                                    actual: instantiated_declared_type.clone_in_current_context(),
                                    expected: expression.item.r#type.clone_in_current_context(),
                                },
                            });
                        }
                    }

                    context.make_progress();

                    ExpressionKind::ResolvedStructure {
                        structure: path,
                        fields,
                    }
                }
                _ => todo!("report error"),
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
        ExpressionKind::Semantics { name, body } => ExpressionKind::Semantics {
            name,
            body: resolve_expression(body.unboxed(), context).boxed(),
        },
        ExpressionKind::Lazy(value) => {
            ExpressionKind::Lazy(resolve_expression(value.unboxed(), context).boxed())
        }
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
    use_expression: WithInfo<D::Info, &mut Expression<D>>,
    context: &ResolveContext<'_, D>,
) -> Result<bool, WithInfo<D::Info, QueuedError<D>>> {
    let item_declaration = context.driver.get_constant_declaration(path);

    let use_type = use_expression.item.r#type.clone_in_current_context();

    // Instantiate the items' type, substituting inferred parameters with opaque
    // type variables

    let instantiated_declared_role = item_declaration.replace(Role::Annotation);

    let instantiation_context = InstantiationContext::from_parameters_with_options(
        context.driver,
        item_declaration.item.parameters,
        context.type_context,
        &use_expression.info,
        context.errors,
        InstantiationOptions {
            instantiate_inferred_parameters_as_opaque: true,
        },
    );

    let mut instantiated_declared_type = infer_type(
        &item_declaration.item.r#type.item,
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

    let _ = unify(context.driver, &mut instantiated_declared_type, &use_type);

    // Check bounds

    let mut evaluate_bounds = {
        let mut instantiated_declared_type = instantiated_declared_type.clone_in_current_context();
        let use_type = &use_type;
        let info = &item_declaration.info;

        move |bounds: &[WithInfo<D::Info, Instance<D>>], allow_unresolved_instances: bool| {
            Ok(bounds
                .iter()
                .map(|bound| {
                    let query = bound.as_ref().map(|bound| Instance {
                        r#trait: bound.r#trait.clone(),
                        parameters: bound
                            .parameters
                            .iter()
                            .map(Type::clone_in_current_context)
                            .collect(),
                    });

                    match resolve_trait(query.as_ref(), context) {
                        Ok(_) => Ok(Some(())),
                        Err(WithInfo {
                            item: QueuedError::UnresolvedInstance { .. },
                            ..
                        }) if allow_unresolved_instances => Ok(None),
                        Err(error) => {
                            // Attempt to get a better error message by unifying the
                            // declared type with the use type again, now that the
                            // bounds have influenced the type
                            if unify(context.driver, &mut instantiated_declared_type, use_type)
                                .is_none()
                            {
                                Err(WithInfo {
                                    info: info.clone(),
                                    item: QueuedError::Mismatch {
                                        actual: use_type.clone_in_current_context(),
                                        expected: instantiated_declared_type
                                            .clone_in_current_context(),
                                    },
                                })
                            } else {
                                Err(error)
                            }
                        }
                    }
                })
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .collect::<Option<()>>()
                .is_some())
        }
    };

    evaluate_bounds(&instantiated_bounds, true)?;

    // Turn the opaque type variables back into regular type variables so they
    // can be inferred now that we've checked the bounds

    let instantiated_declared_type = instantiated_declared_type.instantiate_opaque();

    let instantiated_bounds = instantiated_bounds
        .into_iter()
        .map(|bound| bound.as_ref().map(Instance::instantiate_opaque))
        .collect::<Vec<_>>();

    // Now that we've determined the types of the non-inferred parameters from the bounds,
    // determine the types of the inferred parameters by re-evaluating the bounds

    evaluate_bounds(&instantiated_bounds, true)?;

    try_unify_expression(
        context.driver,
        use_expression,
        &instantiated_declared_type,
        context.error_queue,
    );

    // Evaluate the bounds one more time

    if !evaluate_bounds(&instantiated_bounds, false)? {
        return Ok(false); // try again in the next iteration
    }

    Ok(true)
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
        &trait_declaration.item.r#type.item,
        role,
        Some(context.type_context),
    )
    .instantiate(context.driver, &instantiation_context);

    try_unify_expression(
        context.driver,
        use_expression,
        &trait_type,
        context.error_queue,
    );

    instantiation_context.into_types_for_parameters()
}

struct ResolvedTrait<D: Driver> {
    type_context_change: TypeContextChange<D>,
}

fn resolve_trait<D: Driver>(
    query: WithInfo<D::Info, &Instance<D>>,
    context: &ResolveContext<'_, D>,
) -> Result<WithInfo<D::Info, ResolvedTrait<D>>, WithInfo<D::Info, QueuedError<D>>> {
    type Candidate<D> = (
        WithInfo<<D as Driver>::Info, ResolvedTrait<D>>,
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
                r#trait: query.r#trait.clone(),
                parameters: query
                    .parameters
                    .iter()
                    .map(|r#type| r#type.clone_in_current_context())
                    .collect(),
                candidates: candidates
                    .into_iter()
                    .map(|(candidate, _)| candidate.info)
                    .collect(),
                stack: stack.iter().map(|instance| instance.info.clone()).collect(),
            })),
        }
    }

    fn resolve_trait_inner<D: Driver>(
        query: WithInfo<D::Info, &Instance<D>>,
        context: &ResolveContext<'_, D>,
        stack: &[WithInfo<D::Info, &Instance<D>>],
    ) -> Result<WithInfo<D::Info, ResolvedTrait<D>>, WithInfo<D::Info, QueuedError<D>>> {
        let r#trait = query.item.r#trait.clone();

        let recursion_limit = context.driver.recursion_limit();

        if context.recursion_stack.borrow().len() as u32 > recursion_limit {
            return Err(query.replace(QueuedError::RecursionLimit));
        }

        // First, check if there are any instances in the stack that match -- we
        // assume an instance that refers to itself is satisfied
        for instance in stack.iter().rev() {
            let (new_type_context, type_context_change) = context.type_context.deep_clone();

            let mut query = query
                .as_ref()
                .map(|query| query.clone_in_context(&type_context_change));

            let instance = instance
                .as_ref()
                .map(|instance| instance.clone_in_context(&type_context_change));

            if unify_instance(context.driver, query.as_mut(), instance.as_ref()) {
                let type_context_change = context.type_context.merge(&new_type_context);

                return Ok(WithInfo {
                    info: instance.info,
                    item: ResolvedTrait {
                        type_context_change,
                    },
                });
            }
        }

        // Then, check if there are any bound instances that match
        for bound_instances in context.bound_instances.borrow().iter() {
            let mut candidates = Vec::new();
            for bound_instance in bound_instances {
                let (_, type_context_change) = context.type_context.deep_clone();

                let mut query = query
                    .as_ref()
                    .map(|query| query.clone_in_context(&type_context_change));

                let bound = bound_instance
                    .as_ref()
                    .map(|bound| bound.clone_in_context(&type_context_change));

                if unify_instance_with_options(
                    context.driver,
                    query.as_mut(),
                    bound.as_ref(),
                    UnifyOptions {
                        require_equal_type_parameters: true,
                        ..Default::default()
                    },
                ) {
                    candidates.push((
                        WithInfo {
                            info: bound.info,
                            item: ResolvedTrait {
                                type_context_change,
                            },
                        },
                        Vec::new(),
                    ));
                }
            }

            if let Some((candidate, _)) = pick_from_candidates(candidates, query.clone(), stack)? {
                return Ok(candidate);
            }
        }

        // Next, check if there are any declared instances that match
        let mut candidates = Vec::new();
        for path in context.driver.get_instances_for_trait(&r#trait) {
            let instance_declaration = context.driver.get_instance_declaration(&path);

            let (new_type_context, type_context_change) = context.type_context.deep_clone();

            let mut query = query
                .as_ref()
                .map(|query| query.clone_in_context(&type_context_change));

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
                    infer_type(parameter, None, None)
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

            if unify_instance_with_options(
                context.driver,
                query.as_mut(),
                instance.as_ref(),
                UnifyOptions {
                    require_equal_type_parameters: true,
                    ..Default::default()
                },
            ) {
                candidates.push((
                    WithInfo {
                        info: instance.info,
                        item: ResolvedTrait {
                            type_context_change,
                        },
                    },
                    bounds,
                ));
            }
        }

        // If an instance matches, check its bounds
        if let Some((candidate, bounds)) = pick_from_candidates(candidates, query.clone(), stack)? {
            for bound in bounds {
                let mut stack = stack.to_vec();
                stack.push(bound.as_ref());

                resolve_trait_inner(bound.as_ref(), context, &stack)?;
            }

            return Ok(candidate);
        }

        // If nothing matches, raise an error
        Err(WithInfo {
            info: query.info.clone(),
            item: QueuedError::UnresolvedInstance {
                r#trait: query.item.r#trait.clone(),
                parameters: query
                    .item
                    .parameters
                    .iter()
                    .map(Type::clone_in_current_context)
                    .collect(),
                candidates: Vec::new(),
                stack: stack.iter().map(|instance| instance.info.clone()).collect(),
            },
        })
    }

    resolve_trait_inner(query, context, &[])
}

// region: Finalize

struct FinalizeContext<'a, D: Driver> {
    errors: Option<&'a RefCell<Vec<WithInfo<D::Info, crate::Error<D>>>>>,
}

fn finalize_type<D: Driver>(
    r#type: Type<D>,
    info: &D::Info,
    context: &FinalizeContext<'_, D>,
) -> crate::Type<D> {
    fn finalize_type_inner<D: Driver>(
        mut r#type: Type<D>,
        fully_resolved: &mut bool,
    ) -> crate::Type<D> {
        r#type.apply_in_current_context_mut();

        match r#type.kind {
            TypeKind::Variable(_) | TypeKind::Opaque(_) => {
                *fully_resolved = false;
                crate::Type::Unknown
            }
            TypeKind::Parameter(path) => crate::Type::Parameter(path),
            TypeKind::Declared { path, parameters } => crate::Type::Declared {
                path,
                parameters: parameters
                    .into_iter()
                    .map(|r#type| finalize_type_inner(r#type, fully_resolved))
                    .collect(),
            },
            TypeKind::Function { input, output } => crate::Type::Function {
                input: Box::new(finalize_type_inner(*input, fully_resolved)),
                output: Box::new(finalize_type_inner(*output, fully_resolved)),
            },
            TypeKind::Tuple(elements) => crate::Type::Tuple(
                elements
                    .into_iter()
                    .map(|r#type| finalize_type_inner(r#type, fully_resolved))
                    .collect(),
            ),
            TypeKind::Lazy(r#type) => {
                crate::Type::Lazy(Box::new(finalize_type_inner(*r#type, fully_resolved)))
            }
            TypeKind::Unknown => crate::Type::Unknown,
        }
    }

    let mut fully_resolved = true;
    let finalized_type =
        finalize_type_inner(r#type.clone_in_current_context(), &mut fully_resolved);

    if !fully_resolved {
        if let Some(errors) = &context.errors {
            errors.borrow_mut().push(WithInfo {
                info: info.clone(),
                item: crate::Error::UnknownType(finalized_type.clone()),
            });
        }
    }

    finalized_type
}

fn finalize_expression<D: Driver>(
    expression: WithInfo<D::Info, Expression<D>>,
    context: &FinalizeContext<'_, D>,
) -> WithInfo<D::Info, crate::TypedExpression<D>> {
    let kind = match expression.item.kind {
        ExpressionKind::Unknown(path) => crate::TypedExpressionKind::Unknown(path),
        ExpressionKind::Marker(path) => crate::TypedExpressionKind::Marker(path),
        ExpressionKind::Variable(variable) => crate::TypedExpressionKind::Variable(variable),
        ExpressionKind::UnresolvedConstant(path) | ExpressionKind::UnresolvedTrait(path) => {
            let error = WithInfo {
                info: expression.info.clone(),
                item: crate::Error::UnknownType(finalize_type(
                    expression.item.r#type.clone_in_current_context(),
                    &expression.info,
                    context,
                )),
            };

            if let Some(errors) = &context.errors {
                errors.borrow_mut().push(error);
            }

            crate::TypedExpressionKind::Unknown(Some(path))
        }
        ExpressionKind::ResolvedConstant(path) => crate::TypedExpressionKind::Constant(path),
        ExpressionKind::ResolvedTrait(path) => crate::TypedExpressionKind::Trait(path),
        ExpressionKind::Number(number) => crate::TypedExpressionKind::Number(number),
        ExpressionKind::Text(text) => crate::TypedExpressionKind::Text(text),
        ExpressionKind::Block(statements) => crate::TypedExpressionKind::Block(
            statements
                .into_iter()
                .map(|expression| finalize_expression(expression, context))
                .collect(),
        ),
        ExpressionKind::Function { pattern, body } => crate::TypedExpressionKind::Function {
            pattern,
            body: finalize_expression(body.unboxed(), context).boxed(),
        },
        ExpressionKind::Call { function, input } => crate::TypedExpressionKind::Call {
            function: finalize_expression(function.unboxed(), context).boxed(),
            input: finalize_expression(input.unboxed(), context).boxed(),
        },
        ExpressionKind::When { input, arms } => crate::TypedExpressionKind::When {
            input: finalize_expression(input.unboxed(), context).boxed(),
            arms: arms
                .into_iter()
                .map(|arm| {
                    arm.map(|arm| crate::TypedArm {
                        pattern: arm.pattern,
                        condition: arm
                            .condition
                            .map(|expression| finalize_expression(expression, context)),
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
        ExpressionKind::UnresolvedStructure(_) => {
            let error = WithInfo {
                info: expression.info.clone(),
                item: crate::Error::UnknownType(finalize_type(
                    expression.item.r#type.clone_in_current_context(),
                    &expression.info,
                    context,
                )),
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
                            name: field_value.name,
                            value: finalize_expression(field_value.value, context),
                        })
                    })
                    .collect(),
            }
        }
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
                    text: segment.text,
                    value: finalize_expression(segment.value, context),
                })
                .collect(),
            trailing,
        },
        ExpressionKind::Semantics { name, body } => crate::TypedExpressionKind::Semantics {
            name,
            body: finalize_expression(body.unboxed(), context).boxed(),
        },
        ExpressionKind::Lazy(value) => {
            crate::TypedExpressionKind::Lazy(finalize_expression(value.unboxed(), context).boxed())
        }
    };

    let r#type = finalize_type(expression.item.r#type, &expression.info, context);

    WithInfo {
        info: expression.info,
        item: crate::TypedExpression { r#type, kind },
    }
}
