use crate::{analysis::SpanList, TraitId, TypeId, TypeParameterId};
use serde::Serialize;
use std::{
    cell::{Cell, RefCell},
    collections::BTreeMap,
    hash::Hash,
    rc::Rc,
};

#[derive(Debug, Clone, Serialize)]
pub struct UnresolvedType {
    pub span: Option<SpanList>,
    pub kind: UnresolvedTypeKind,
}

impl PartialEq for UnresolvedType {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Eq for UnresolvedType {}

impl Hash for UnresolvedType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub enum UnresolvedTypeKind {
    Variable(TypeVariable),
    Parameter(TypeParameterId),
    NumericVariable(TypeVariable),
    Named(TypeId, Vec<UnresolvedType>, TypeStructure<UnresolvedType>),
    Function(Box<UnresolvedType>, Box<UnresolvedType>),
    Tuple(Vec<UnresolvedType>),
    Builtin(BuiltinType<Box<UnresolvedType>>),
    Error,
}

impl UnresolvedTypeKind {
    pub fn with_span(self, span: impl Into<Option<SpanList>>) -> UnresolvedType {
        UnresolvedType {
            span: span.into(),
            kind: self,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct Type {
    pub span: Option<SpanList>,
    pub kind: TypeKind,
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Eq for Type {}

impl Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub enum TypeKind {
    Parameter(TypeParameterId),
    Named(TypeId, Vec<Type>, TypeStructure<Type>),
    Function(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Builtin(BuiltinType<Box<Type>>),
    Error,
}

impl TypeKind {
    pub fn with_span(self, span: impl Into<Option<SpanList>>) -> Type {
        Type {
            span: span.into(),
            kind: self,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub enum TypeStructure<Ty> {
    Marker,
    Structure(Vec<Ty>),
    Enumeration(Vec<Vec<Ty>>),
    Recursive(TypeId),
}

impl From<Type> for UnresolvedType {
    fn from(ty: Type) -> Self {
        UnresolvedType {
            span: ty.span,
            kind: match ty.kind {
                TypeKind::Parameter(param) => UnresolvedTypeKind::Parameter(param),
                TypeKind::Named(id, params, structure) => UnresolvedTypeKind::Named(
                    id,
                    params.into_iter().map(|param| param.into()).collect(),
                    structure.into(),
                ),
                TypeKind::Function(input, output) => UnresolvedTypeKind::Function(
                    Box::new((*input).into()),
                    Box::new((*output).into()),
                ),
                TypeKind::Tuple(tys) => {
                    UnresolvedTypeKind::Tuple(tys.into_iter().map(|ty| ty.into()).collect())
                }
                TypeKind::Builtin(builtin) => UnresolvedTypeKind::Builtin(match builtin {
                    BuiltinType::Number => BuiltinType::Number,
                    BuiltinType::Integer => BuiltinType::Integer,
                    BuiltinType::Natural => BuiltinType::Natural,
                    BuiltinType::Byte => BuiltinType::Byte,
                    BuiltinType::Signed => BuiltinType::Signed,
                    BuiltinType::Unsigned => BuiltinType::Unsigned,
                    BuiltinType::Float => BuiltinType::Float,
                    BuiltinType::Double => BuiltinType::Double,
                    BuiltinType::Text => BuiltinType::Text,
                    BuiltinType::List(ty) => BuiltinType::List(Box::new((*ty).into())),
                    BuiltinType::Mutable(ty) => BuiltinType::Mutable(Box::new((*ty).into())),
                    BuiltinType::Ui => BuiltinType::Ui,
                    BuiltinType::TaskGroup => BuiltinType::TaskGroup,
                }),
                TypeKind::Error => UnresolvedTypeKind::Error,
            },
        }
    }
}

impl From<TypeStructure<Type>> for TypeStructure<UnresolvedType> {
    fn from(structure: TypeStructure<Type>) -> Self {
        match structure {
            TypeStructure::Marker => TypeStructure::Marker,
            TypeStructure::Structure(tys) => {
                TypeStructure::Structure(tys.into_iter().map(From::from).collect())
            }
            TypeStructure::Enumeration(variants) => TypeStructure::Enumeration(
                variants
                    .into_iter()
                    .map(|tys| tys.into_iter().map(From::from).collect())
                    .collect(),
            ),
            TypeStructure::Recursive(id) => TypeStructure::Recursive(id),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct TypeVariable(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub enum BuiltinType<Ty> {
    Number,
    Integer,
    Natural,
    Byte,
    Signed,
    Unsigned,
    Float,
    Double,
    Text,
    List(Ty),
    Mutable(Ty),
    Ui,
    TaskGroup,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BottomTypeReason {
    Annotated,
    Error,
}

pub type GenericSubstitutions = BTreeMap<TypeParameterId, UnresolvedType>;
pub type FinalizedGenericSubstitutions = BTreeMap<TypeParameterId, Type>;

#[derive(Debug, Clone, Default)]
pub struct Context {
    pub next_var: Cell<usize>,
    pub substitutions: RefCell<im::HashMap<TypeVariable, UnresolvedType>>,
    pub defaults: RefCell<im::HashMap<TypeVariable, UnresolvedType>>,
    pub numeric_substitutions: RefCell<im::HashMap<TypeVariable, UnresolvedType>>,
}

#[derive(Debug, Clone)]
pub enum TypeError {
    ErrorExpression,
    Recursive(TypeVariable),
    Mismatch(UnresolvedType, UnresolvedType),
    MissingInstance(
        TraitId,
        Vec<UnresolvedType>,
        Option<SpanList>,
        Vec<SpanList>,
    ),
    UnresolvedType(UnresolvedType),
    InvalidNumericLiteral(UnresolvedType),
}

pub type Result<T> = std::result::Result<T, Box<TypeError>>;

impl Context {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn new_variable(&self, default: Option<UnresolvedType>) -> TypeVariable {
        let next_var = self.next_var.get();
        let var = TypeVariable(next_var);

        if let Some(default) = default {
            self.defaults.borrow_mut().insert(var, default);
        }

        self.next_var.set(next_var + 1);

        var
    }

    pub fn unify_params(
        &self,
        actual: UnresolvedType,
        expected: impl Into<UnresolvedType>,
        get_default: impl Fn(TypeParameterId) -> Option<UnresolvedType>,
    ) -> GenericSubstitutions {
        let params: Rc<RefCell<GenericSubstitutions>> = Default::default();
        let _ = self.unify_internal(
            actual,
            expected.into(),
            false,
            false,
            Some((params.clone(), Rc::new(get_default))),
        );

        Rc::into_inner(params).unwrap().into_inner()
    }

    pub fn unify(&self, actual: UnresolvedType, expected: impl Into<UnresolvedType>) -> Result<()> {
        self.unify_internal(actual, expected.into(), false, false, None)
    }

    pub fn unify_reverse(
        &self,
        actual: impl Into<UnresolvedType>,
        expected: UnresolvedType,
    ) -> Result<()> {
        self.unify_internal(actual.into(), expected, false, true, None)
    }

    pub fn unify_generic(
        &self,
        actual: UnresolvedType,
        expected: impl Into<UnresolvedType>,
    ) -> Result<()> {
        self.unify_internal(actual, expected.into(), true, false, None)
    }

    fn unify_internal(
        &self,
        mut actual: UnresolvedType,
        mut expected: UnresolvedType,
        generic: bool,
        reverse: bool,
        params: Option<(
            Rc<RefCell<BTreeMap<TypeParameterId, UnresolvedType>>>,
            Rc<dyn Fn(TypeParameterId) -> Option<UnresolvedType> + '_>,
        )>,
    ) -> Result<()> {
        actual.apply(self);
        expected.apply(self);

        if let Some((params, _)) = &params {
            if let UnresolvedTypeKind::Parameter(param) = expected.kind {
                params.borrow_mut().insert(param, actual.clone());
            }
        }

        let mismatch = || {
            if reverse {
                Box::new(TypeError::Mismatch(expected.clone(), actual.clone()))
            } else {
                Box::new(TypeError::Mismatch(actual.clone(), expected.clone()))
            }
        };

        macro_rules! unify_var {
            ($var:ident, $ty:ident) => {{
                let var = $var;
                let ty = $ty;

                if let UnresolvedTypeKind::Variable(other) = ty.kind {
                    if var == other {
                        return Ok(());
                    }
                }

                if ty.contains(&var) {
                    if params.is_none() {
                        Err(Box::new(TypeError::Recursive(var)))
                    } else {
                        Ok(())
                    }
                } else {
                    if let UnresolvedTypeKind::Variable(other) = ty.kind {
                        let mut defaults = self.defaults.borrow_mut();

                        if let Some(source) = defaults.get(&var).cloned() {
                            defaults.insert(other, source);
                        } else if let Some(default) = defaults.get(&other).cloned() {
                            defaults.insert(var, default);
                        }
                    }

                    self.substitutions.borrow_mut().insert(var, ty);

                    Ok(())
                }
            }};
        }

        match (actual.kind.clone(), expected.kind.clone()) {
            (
                UnresolvedTypeKind::Parameter(actual_param),
                UnresolvedTypeKind::Parameter(expected_param),
            ) if generic => {
                if actual_param == expected_param {
                    Ok(())
                } else if params.is_none() {
                    Err(mismatch())
                } else {
                    Ok(())
                }
            }
            (ty, UnresolvedTypeKind::Parameter(param)) if !generic => {
                if let UnresolvedTypeKind::Variable(var) = ty {
                    if let Some((_, default)) = params {
                        if let Some(ty) = default(param) {
                            self.defaults.borrow_mut().insert(var, ty);
                        }
                    }
                }

                Ok(())
            }
            // FIXME: Determine if removing this is sound
            // (UnresolvedTypeKind::Parameter(actual), expected) if !generic => {
            //     Err(mismatch!(UnresolvedTypeKind::Parameter(actual), expected))
            // }
            (UnresolvedTypeKind::Variable(var), _) => unify_var!(var, expected),
            (_, UnresolvedTypeKind::Variable(var)) => unify_var!(var, actual),
            (UnresolvedTypeKind::NumericVariable(var), expected_kind) => {
                match expected_kind {
                    UnresolvedTypeKind::NumericVariable(other) => {
                        if var == other {
                            return Ok(());
                        }
                    }
                    UnresolvedTypeKind::Builtin(ty) if ty.is_numeric() => {}
                    _ => {
                        if params.is_none() {
                            return Err(mismatch());
                        }
                    }
                }

                if expected.contains(&var) {
                    if params.is_none() {
                        Err(Box::new(TypeError::Recursive(var)))
                    } else {
                        Ok(())
                    }
                } else {
                    self.numeric_substitutions
                        .borrow_mut()
                        .insert(var, expected);
                    Ok(())
                }
            }
            (actual_kind, UnresolvedTypeKind::NumericVariable(var)) => {
                match actual_kind {
                    UnresolvedTypeKind::NumericVariable(other) => {
                        if var == other {
                            return Ok(());
                        }
                    }
                    UnresolvedTypeKind::Builtin(ty) if ty.is_numeric() => {}
                    _ => {
                        if params.is_none() {
                            return Err(mismatch());
                        }
                    }
                }

                if actual.contains(&var) {
                    if params.is_none() {
                        Err(Box::new(TypeError::Recursive(var)))
                    } else {
                        Ok(())
                    }
                } else {
                    self.numeric_substitutions.borrow_mut().insert(var, actual);
                    Ok(())
                }
            }
            (
                UnresolvedTypeKind::Named(actual_id, actual_params, _),
                UnresolvedTypeKind::Named(expected_id, expected_params, _),
            ) => {
                if actual_id == expected_id {
                    let mut error = false;
                    for (actual, expected) in actual_params.iter().zip(&expected_params) {
                        if let Err(e) = self.unify_internal(
                            actual.clone(),
                            expected.clone(),
                            generic,
                            reverse,
                            params.clone(),
                        ) {
                            if let TypeError::Mismatch(_, _) = *e {
                                error = true;
                            } else if params.is_none() {
                                return Err(e);
                            }
                        }
                    }

                    if error && params.is_none() {
                        return Err(mismatch());
                    }

                    Ok(())
                } else if params.is_none() {
                    Err(mismatch())
                } else {
                    Ok(())
                }
            }
            (
                UnresolvedTypeKind::Function(actual_input, actual_output),
                UnresolvedTypeKind::Function(expected_input, expected_output),
            ) => {
                let mut error = false;

                if let Err(e) = self.unify_internal(
                    (*actual_input).clone(),
                    (*expected_input).clone(),
                    generic,
                    reverse,
                    params.clone(),
                ) {
                    if let TypeError::Mismatch(_, _) = *e {
                        error = true;
                    } else if params.is_none() {
                        return Err(e);
                    }
                }

                if let Err(e) = self.unify_internal(
                    (*actual_output).clone(),
                    (*expected_output).clone(),
                    generic,
                    reverse,
                    params.clone(),
                ) {
                    if let TypeError::Mismatch(_, _) = *e {
                        error = true;
                    } else if params.is_none() {
                        return Err(e);
                    }
                }

                if error && params.is_none() {
                    return Err(mismatch());
                }

                Ok(())
            }
            (UnresolvedTypeKind::Tuple(actual_tys), UnresolvedTypeKind::Tuple(expected_tys)) => {
                if actual_tys.len() != expected_tys.len() && params.is_none() {
                    return Err(mismatch());
                }

                let mut error = false;
                for (actual, expected) in std::iter::zip(&actual_tys, &expected_tys) {
                    if let Err(e) = self.unify_internal(
                        actual.clone(),
                        expected.clone(),
                        generic,
                        reverse,
                        params.clone(),
                    ) {
                        if let TypeError::Mismatch(_, _) = *e {
                            error = true;
                        } else if params.is_none() {
                            return Err(e);
                        }
                    }
                }

                if error && params.is_none() {
                    return Err(mismatch());
                }

                Ok(())
            }
            (
                UnresolvedTypeKind::Builtin(actual_builtin),
                UnresolvedTypeKind::Builtin(expected_builtin),
            ) => match (actual_builtin, expected_builtin) {
                (BuiltinType::Number, BuiltinType::Number)
                | (BuiltinType::Integer, BuiltinType::Integer)
                | (BuiltinType::Natural, BuiltinType::Natural)
                | (BuiltinType::Byte, BuiltinType::Byte)
                | (BuiltinType::Signed, BuiltinType::Signed)
                | (BuiltinType::Unsigned, BuiltinType::Unsigned)
                | (BuiltinType::Float, BuiltinType::Float)
                | (BuiltinType::Double, BuiltinType::Double)
                | (BuiltinType::Text, BuiltinType::Text)
                | (BuiltinType::Ui, BuiltinType::Ui)
                | (BuiltinType::TaskGroup, BuiltinType::TaskGroup) => Ok(()),
                (BuiltinType::List(actual_element), BuiltinType::List(expected_element)) => {
                    if let Err(error) = self.unify_internal(
                        (*actual_element).clone(),
                        (*expected_element).clone(),
                        generic,
                        reverse,
                        params.clone(),
                    ) {
                        if params.is_none() {
                            return Err(if let TypeError::Mismatch(_, _) = *error {
                                mismatch()
                            } else {
                                error
                            });
                        }
                    }

                    Ok(())
                }
                (BuiltinType::Mutable(actual_element), BuiltinType::Mutable(expected_element)) => {
                    if let Err(error) = self.unify_internal(
                        (*actual_element).clone(),
                        (*expected_element).clone(),
                        generic,
                        reverse,
                        params.clone(),
                    ) {
                        if params.is_none() {
                            return Err(if let TypeError::Mismatch(_, _) = *error {
                                mismatch()
                            } else {
                                error
                            });
                        }
                    }

                    Ok(())
                }
                _ => {
                    if params.is_none() {
                        Err(mismatch())
                    } else {
                        Ok(())
                    }
                }
            },
            (_, UnresolvedTypeKind::Error) | (UnresolvedTypeKind::Error, _) => Ok(()),
            _ => {
                if params.is_none() {
                    Err(mismatch())
                } else {
                    Ok(())
                }
            }
        }
    }
}

impl UnresolvedType {
    pub fn id(&self) -> Option<TypeId> {
        match &self.kind {
            UnresolvedTypeKind::Named(id, _, _) => Some(*id),
            _ => None,
        }
    }

    pub fn contains(&self, var: &TypeVariable) -> bool {
        match &self.kind {
            UnresolvedTypeKind::Variable(v) | UnresolvedTypeKind::NumericVariable(v) => v == var,
            UnresolvedTypeKind::Function(input, output) => {
                input.contains(var) || output.contains(var)
            }
            UnresolvedTypeKind::Named(_, params, _) => {
                params.iter().any(|param| param.contains(var))
            }
            UnresolvedTypeKind::Tuple(tys) => tys.iter().any(|ty| ty.contains(var)),
            UnresolvedTypeKind::Builtin(ty) => match ty {
                BuiltinType::List(ty) | BuiltinType::Mutable(ty) => ty.contains(var),
                _ => false,
            },
            _ => false,
        }
    }

    pub fn contains_error(&self) -> bool {
        match &self.kind {
            UnresolvedTypeKind::Function(input, output) => {
                input.contains_error() || output.contains_error()
            }
            UnresolvedTypeKind::Error => true,
            UnresolvedTypeKind::Named(_, params, _) => {
                params.iter().any(|param| param.contains_error())
            }
            UnresolvedTypeKind::Tuple(tys) => tys.iter().any(|ty| ty.contains_error()),
            UnresolvedTypeKind::Builtin(ty) => match ty {
                BuiltinType::List(ty) | BuiltinType::Mutable(ty) => ty.contains_error(),
                _ => false,
            },
            _ => false,
        }
    }

    pub fn contains_vars(&self) -> bool {
        match &self.kind {
            UnresolvedTypeKind::Variable(_) => true,
            UnresolvedTypeKind::Function(input, output) => {
                input.contains_vars() || output.contains_vars()
            }
            UnresolvedTypeKind::Named(_, params, _) => {
                params.iter().any(|param| param.contains_vars())
            }
            UnresolvedTypeKind::Tuple(tys) => tys.iter().any(|ty| ty.contains_vars()),
            UnresolvedTypeKind::Builtin(ty) => match ty {
                BuiltinType::List(ty) | BuiltinType::Mutable(ty) => ty.contains_vars(),
                _ => false,
            },
            _ => false,
        }
    }

    pub fn apply(&mut self, ctx: &Context) {
        match &mut self.kind {
            UnresolvedTypeKind::Variable(var) => {
                if let Some(ty) = ctx.substitutions.borrow().get(var).cloned() {
                    self.kind = ty.kind;
                    self.apply(ctx);
                }
            }
            UnresolvedTypeKind::NumericVariable(var) => {
                if let Some(ty) = ctx.numeric_substitutions.borrow().get(var).cloned() {
                    self.kind = ty.kind;
                    self.apply(ctx);
                }
            }
            UnresolvedTypeKind::Function(input, output) => {
                input.apply(ctx);
                output.apply(ctx);
            }
            UnresolvedTypeKind::Named(_, params, structure) => {
                for param in params {
                    param.apply(ctx);
                }

                structure.apply(ctx);
            }
            UnresolvedTypeKind::Tuple(tys) => {
                for ty in tys {
                    ty.apply(ctx);
                }
            }
            UnresolvedTypeKind::Builtin(ty) => match ty {
                BuiltinType::List(ty) | BuiltinType::Mutable(ty) => ty.apply(ctx),
                _ => {}
            },
            _ => {}
        }
    }

    pub fn instantiate_with(&mut self, ctx: &Context, substitutions: &GenericSubstitutions) {
        self.apply(ctx);

        match &mut self.kind {
            UnresolvedTypeKind::Parameter(param) => {
                self.kind = substitutions
                    .get(param)
                    .map(|ty| ty.kind.clone())
                    .unwrap_or_else(|| {
                        // HACK: If the typechecker behaves erratically, try panicking here instead
                        // of returning a new type variable to get to the root cause earlier.
                        UnresolvedTypeKind::Variable(ctx.new_variable(None))
                    });
            }
            UnresolvedTypeKind::Function(input, output) => {
                input.instantiate_with(ctx, substitutions);
                output.instantiate_with(ctx, substitutions);
            }
            UnresolvedTypeKind::Named(_, params, structure) => {
                for param in params {
                    param.instantiate_with(ctx, substitutions);
                }

                structure.instantiate_with(ctx, substitutions);
            }
            UnresolvedTypeKind::Tuple(tys) => {
                for ty in tys {
                    ty.instantiate_with(ctx, substitutions);
                }
            }
            UnresolvedTypeKind::Builtin(ty) => match ty {
                BuiltinType::List(ty) | BuiltinType::Mutable(ty) => {
                    ty.instantiate_with(ctx, substitutions);
                }
                _ => {}
            },
            _ => {}
        }
    }

    pub fn vars(&self) -> Vec<TypeVariable> {
        match &self.kind {
            UnresolvedTypeKind::Variable(var) => vec![*var],
            UnresolvedTypeKind::Function(input, output) => {
                let mut vars = input.vars();
                vars.extend(output.vars());
                vars
            }
            UnresolvedTypeKind::Named(_, params, structure) => params
                .iter()
                .flat_map(|ty| ty.vars())
                .chain(structure.vars())
                .collect(),
            UnresolvedTypeKind::Tuple(tys) => tys.iter().flat_map(|ty| ty.vars()).collect(),
            UnresolvedTypeKind::Builtin(ty) => match ty {
                BuiltinType::List(ty) | BuiltinType::Mutable(ty) => ty.vars(),
                _ => Vec::new(),
            },
            _ => Vec::new(),
        }
    }

    pub fn visible_vars(&self) -> Vec<TypeVariable> {
        match &self.kind {
            UnresolvedTypeKind::Variable(var) => vec![*var],
            UnresolvedTypeKind::Function(input, output) => {
                let mut vars = input.visible_vars();
                vars.extend(output.visible_vars());
                vars
            }
            UnresolvedTypeKind::Named(_, params, _) => {
                params.iter().flat_map(|ty| ty.visible_vars()).collect()
            }
            UnresolvedTypeKind::Tuple(tys) => tys.iter().flat_map(|ty| ty.visible_vars()).collect(),
            UnresolvedTypeKind::Builtin(ty) => match ty {
                BuiltinType::List(ty) | BuiltinType::Mutable(ty) => ty.visible_vars(),
                _ => Vec::new(),
            },
            _ => Vec::new(),
        }
    }

    pub fn all_vars(&self) -> Vec<TypeVariable> {
        match &self.kind {
            UnresolvedTypeKind::Variable(var) | UnresolvedTypeKind::NumericVariable(var) => {
                vec![*var]
            }
            UnresolvedTypeKind::Function(input, output) => {
                let mut vars = input.all_vars();
                vars.extend(output.all_vars());
                vars
            }
            UnresolvedTypeKind::Named(_, params, structure) => params
                .iter()
                .flat_map(|ty| ty.all_vars())
                .chain(structure.all_vars())
                .collect(),
            UnresolvedTypeKind::Tuple(tys) => tys.iter().flat_map(|ty| ty.all_vars()).collect(),
            UnresolvedTypeKind::Builtin(ty) => match ty {
                BuiltinType::List(ty) | BuiltinType::Mutable(ty) => ty.all_vars(),
                _ => Vec::new(),
            },
            _ => Vec::new(),
        }
    }

    pub fn params(&self) -> Vec<TypeParameterId> {
        match &self.kind {
            UnresolvedTypeKind::Parameter(param) => vec![*param],
            UnresolvedTypeKind::Function(input, output) => {
                let mut params = input.params();
                params.extend(output.params());
                params
            }
            UnresolvedTypeKind::Named(_, params, structure) => params
                .iter()
                .flat_map(|ty| ty.params())
                .chain(structure.params())
                .collect(),
            UnresolvedTypeKind::Tuple(tys) => tys.iter().flat_map(|ty| ty.params()).collect(),
            UnresolvedTypeKind::Builtin(ty) => match ty {
                BuiltinType::List(ty) | BuiltinType::Mutable(ty) => ty.params(),
                _ => Vec::new(),
            },
            _ => Vec::new(),
        }
    }

    pub fn substitute_defaults(&mut self, ctx: &Context) {
        self.apply(ctx);

        match &mut self.kind {
            UnresolvedTypeKind::Variable(var) => {
                if let Some(default) = ctx.defaults.borrow().get(var).cloned() {
                    ctx.substitutions.borrow_mut().insert(*var, default.clone());
                    self.substitute_defaults(ctx);
                }
            }
            UnresolvedTypeKind::Function(input, output) => {
                input.substitute_defaults(ctx);
                output.substitute_defaults(ctx);
            }
            UnresolvedTypeKind::Named(_, params, structure) => {
                for param in params {
                    param.substitute_defaults(ctx);
                }

                structure.finalize_defaults(ctx);
            }
            UnresolvedTypeKind::Tuple(tys) => {
                for ty in tys {
                    ty.substitute_defaults(ctx);
                }
            }
            UnresolvedTypeKind::Builtin(ty) => match ty {
                BuiltinType::List(ty) | BuiltinType::Mutable(ty) => {
                    ty.substitute_defaults(ctx);
                }
                _ => {}
            },
            _ => {}
        }
    }

    pub fn finalize_numeric_variables(&mut self, ctx: &Context) {
        self.apply(ctx);

        match &mut self.kind {
            UnresolvedTypeKind::NumericVariable(var) => {
                if let Some(ty) = ctx.numeric_substitutions.borrow().get(var).cloned() {
                    self.kind = ty.kind;
                    self.finalize_numeric_variables(ctx);
                } else {
                    self.kind = UnresolvedTypeKind::Builtin(BuiltinType::Number);
                }
            }
            UnresolvedTypeKind::Function(input, output) => {
                input.finalize_numeric_variables(ctx);
                output.finalize_numeric_variables(ctx);
            }
            UnresolvedTypeKind::Named(_, params, structure) => {
                for param in params {
                    param.finalize_numeric_variables(ctx);
                }

                structure.finalize_numeric_variables(ctx);
            }
            UnresolvedTypeKind::Tuple(tys) => {
                for ty in tys {
                    ty.finalize_numeric_variables(ctx);
                }
            }
            UnresolvedTypeKind::Builtin(ty) => match ty {
                BuiltinType::List(ty) | BuiltinType::Mutable(ty) => {
                    ty.finalize_numeric_variables(ctx)
                }
                _ => {}
            },
            _ => {}
        }
    }

    pub fn finalize(&self, ctx: &Context) -> (Type, bool) {
        let mut resolved = true;
        let ty = self.finalize_inner(ctx, &mut resolved);
        (ty, resolved)
    }

    fn finalize_inner(&self, ctx: &Context, resolved: &mut bool) -> Type {
        let mut ty = self.clone();
        ty.apply(ctx);
        ty.substitute_defaults(ctx);
        ty.finalize_numeric_variables(ctx);

        Type {
            span: ty.span,
            kind: match ty.kind.clone() {
                UnresolvedTypeKind::Variable(_) => {
                    *resolved = false;
                    TypeKind::Error
                }
                UnresolvedTypeKind::Parameter(param) => TypeKind::Parameter(param),
                UnresolvedTypeKind::NumericVariable(_) => unreachable!(),
                UnresolvedTypeKind::Named(id, params, structure) => TypeKind::Named(
                    id,
                    params
                        .into_iter()
                        .map(|param| param.finalize_inner(ctx, resolved))
                        .collect(),
                    structure.finalize_inner(ctx, resolved),
                ),
                UnresolvedTypeKind::Function(input, output) => TypeKind::Function(
                    Box::new(input.finalize_inner(ctx, resolved)),
                    Box::new(output.finalize_inner(ctx, resolved)),
                ),
                UnresolvedTypeKind::Tuple(tys) => TypeKind::Tuple(
                    tys.into_iter()
                        .map(|ty| ty.finalize_inner(ctx, resolved))
                        .collect(),
                ),
                UnresolvedTypeKind::Builtin(builtin) => TypeKind::Builtin(match builtin {
                    BuiltinType::Number => BuiltinType::Number,
                    BuiltinType::Integer => BuiltinType::Integer,
                    BuiltinType::Natural => BuiltinType::Natural,
                    BuiltinType::Byte => BuiltinType::Byte,
                    BuiltinType::Signed => BuiltinType::Signed,
                    BuiltinType::Unsigned => BuiltinType::Unsigned,
                    BuiltinType::Float => BuiltinType::Float,
                    BuiltinType::Double => BuiltinType::Double,
                    BuiltinType::Text => BuiltinType::Text,
                    BuiltinType::List(ty) => {
                        BuiltinType::List(Box::new(ty.finalize_inner(ctx, resolved)))
                    }
                    BuiltinType::Mutable(ty) => {
                        BuiltinType::Mutable(Box::new(ty.finalize_inner(ctx, resolved)))
                    }
                    BuiltinType::Ui => BuiltinType::Ui,
                    BuiltinType::TaskGroup => BuiltinType::TaskGroup,
                }),
                UnresolvedTypeKind::Error => TypeKind::Error,
            },
        }
    }
}

impl TypeStructure<UnresolvedType> {
    pub fn apply(&mut self, ctx: &Context) {
        match self {
            TypeStructure::Marker | TypeStructure::Recursive(_) => {}
            TypeStructure::Structure(tys) => {
                for ty in tys {
                    ty.apply(ctx);
                }
            }
            TypeStructure::Enumeration(variants) => {
                for tys in variants {
                    for ty in tys {
                        ty.apply(ctx);
                    }
                }
            }
        }
    }

    pub fn instantiate_with(&mut self, ctx: &Context, substitutions: &GenericSubstitutions) {
        match self {
            TypeStructure::Marker | TypeStructure::Recursive(_) => {}
            TypeStructure::Structure(tys) => {
                for ty in tys {
                    ty.instantiate_with(ctx, substitutions);
                }
            }
            TypeStructure::Enumeration(variants) => {
                for tys in variants {
                    for ty in tys {
                        ty.instantiate_with(ctx, substitutions);
                    }
                }
            }
        }
    }

    pub fn vars(&self) -> Vec<TypeVariable> {
        match self {
            TypeStructure::Marker | TypeStructure::Recursive(_) => Vec::new(),
            TypeStructure::Structure(tys) => tys.iter().flat_map(|ty| ty.vars()).collect(),
            TypeStructure::Enumeration(variants) => variants
                .iter()
                .flat_map(|tys| tys.iter().flat_map(|ty| ty.vars()))
                .collect(),
        }
    }

    pub fn all_vars(&self) -> Vec<TypeVariable> {
        match self {
            TypeStructure::Marker | TypeStructure::Recursive(_) => Vec::new(),
            TypeStructure::Structure(tys) => tys.iter().flat_map(|ty| ty.all_vars()).collect(),
            TypeStructure::Enumeration(variants) => variants
                .iter()
                .flat_map(|tys| tys.iter().flat_map(|ty| ty.all_vars()))
                .collect(),
        }
    }

    pub fn params(&self) -> Vec<TypeParameterId> {
        match self {
            TypeStructure::Marker | TypeStructure::Recursive(_) => Vec::new(),
            TypeStructure::Structure(tys) => tys.iter().flat_map(|ty| ty.params()).collect(),
            TypeStructure::Enumeration(variants) => variants
                .iter()
                .flat_map(|tys| tys.iter().flat_map(|ty| ty.params()))
                .collect(),
        }
    }

    pub fn finalize_defaults(&mut self, ctx: &Context) {
        match self {
            TypeStructure::Marker | TypeStructure::Recursive(_) => {}
            TypeStructure::Structure(tys) => {
                for ty in tys {
                    ty.substitute_defaults(ctx);
                }
            }
            TypeStructure::Enumeration(variants) => {
                for tys in variants {
                    for ty in tys {
                        ty.substitute_defaults(ctx);
                    }
                }
            }
        }
    }

    pub fn finalize_numeric_variables(&mut self, ctx: &Context) {
        match self {
            TypeStructure::Marker | TypeStructure::Recursive(_) => {}
            TypeStructure::Structure(tys) => {
                for ty in tys {
                    ty.finalize_numeric_variables(ctx);
                }
            }
            TypeStructure::Enumeration(variants) => {
                for tys in variants {
                    for ty in tys {
                        ty.finalize_numeric_variables(ctx);
                    }
                }
            }
        }
    }

    fn finalize_inner(self, ctx: &Context, resolved: &mut bool) -> TypeStructure<Type> {
        match self {
            TypeStructure::Marker => TypeStructure::Marker,
            TypeStructure::Structure(tys) => TypeStructure::Structure(
                tys.into_iter()
                    .map(|ty| ty.finalize_inner(ctx, resolved))
                    .collect(),
            ),
            TypeStructure::Enumeration(variants) => TypeStructure::Enumeration(
                variants
                    .into_iter()
                    .map(|tys| {
                        tys.into_iter()
                            .map(|ty| ty.finalize_inner(ctx, resolved))
                            .collect()
                    })
                    .collect(),
            ),
            TypeStructure::Recursive(id) => TypeStructure::Recursive(id),
        }
    }
}

impl TypeStructure<Type> {
    pub fn params(&self) -> Vec<TypeParameterId> {
        match self {
            TypeStructure::Marker | TypeStructure::Recursive(_) => Vec::new(),
            TypeStructure::Structure(tys) => tys.iter().flat_map(|ty| ty.params()).collect(),
            TypeStructure::Enumeration(variants) => variants
                .iter()
                .flat_map(|tys| tys.iter().flat_map(|ty| ty.params()))
                .collect(),
        }
    }

    pub fn instantiate_with(&mut self, substitutions: &FinalizedGenericSubstitutions) {
        match self {
            TypeStructure::Marker | TypeStructure::Recursive(_) => {}
            TypeStructure::Structure(tys) => {
                for ty in tys {
                    ty.instantiate_with(substitutions);
                }
            }
            TypeStructure::Enumeration(variants) => {
                for tys in variants {
                    for ty in tys {
                        ty.instantiate_with(substitutions);
                    }
                }
            }
        }
    }
}

impl Type {
    pub fn params(&self) -> Vec<TypeParameterId> {
        match &self.kind {
            TypeKind::Parameter(param) => vec![*param],
            TypeKind::Function(input, output) => {
                let mut params = input.params();
                params.extend(output.params());
                params
            }
            TypeKind::Named(_, params, structure) => params
                .iter()
                .flat_map(|ty| ty.params())
                .chain(structure.params())
                .collect(),
            TypeKind::Tuple(tys) => tys.iter().flat_map(|ty| ty.params()).collect(),
            TypeKind::Builtin(ty) => match ty {
                BuiltinType::List(ty) | BuiltinType::Mutable(ty) => ty.params(),
                _ => Vec::new(),
            },
            _ => Vec::new(),
        }
    }

    pub fn instantiate_with(&mut self, substitutions: &FinalizedGenericSubstitutions) {
        match &mut self.kind {
            TypeKind::Parameter(param) => {
                if let Some(ty) = substitutions.get(param).cloned() {
                    self.kind = ty.kind;
                }
            }
            TypeKind::Function(input, output) => {
                input.instantiate_with(substitutions);
                output.instantiate_with(substitutions);
            }
            TypeKind::Named(_, params, structure) => {
                for param in params {
                    param.instantiate_with(substitutions);
                }

                structure.instantiate_with(substitutions);
            }
            TypeKind::Tuple(tys) => {
                for ty in tys {
                    ty.instantiate_with(substitutions);
                }
            }
            TypeKind::Builtin(ty) => match ty {
                BuiltinType::List(ty) | BuiltinType::Mutable(ty) => {
                    ty.instantiate_with(substitutions)
                }
                _ => {}
            },
            _ => {}
        }
    }
}

impl<Ty> BuiltinType<Ty> {
    fn is_numeric(&self) -> bool {
        matches!(
            self,
            BuiltinType::Number
                | BuiltinType::Integer
                | BuiltinType::Natural
                | BuiltinType::Byte
                | BuiltinType::Signed
                | BuiltinType::Unsigned
                | BuiltinType::Float
                | BuiltinType::Double
        )
    }
}
