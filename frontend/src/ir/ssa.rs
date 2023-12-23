use crate::{
    analysis, analysis::SpanList, helpers::InternedString, Compiler, ConstantId, EnumerationId,
    FieldIndex, ItemId, StructureId, TypeId, VariableId, VariantIndex,
};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

pub use crate::analysis::Intrinsic;

#[derive(Debug, Clone)]
pub struct Program {
    pub items: BTreeMap<ItemId, Expression>,
    pub contexts: BTreeMap<ConstantId, (Type, ItemId)>,
    pub variables: BTreeMap<VariableId, Type>,
    pub structures: BTreeMap<StructureId, Vec<Type>>,
    pub enumerations: BTreeMap<EnumerationId, Vec<Vec<Type>>>,
    pub entrypoint: ItemId,
    pub entrypoint_wrapper: Option<ItemId>,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub span: Option<SpanList>,
    pub tail: bool,
    pub ty: Type,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Type {
    Marker,
    Number,
    Ui,
    TaskGroup,
    Hasher,
    Tuple(Vec<Type>),
    Structure(StructureId),
    Enumeration(EnumerationId),
    TextReference,
    ListReference(Box<Type>),
    MutableReference(Box<Type>),
    FunctionReference(Box<Type>, Box<Type>),
    StructureReference(StructureId),
    EnumerationReference(EnumerationId),
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Marker,
    Variable(VariableId),
    Text(InternedString),
    Block(Vec<Expression>),
    Call(Box<Expression>, Box<Expression>),
    Function(Pattern, Box<Expression>, Vec<(VariableId, Type)>),
    When(Box<Expression>, Vec<Arm>),
    External(InternedString, InternedString, Vec<Expression>),
    Intrinsic(Intrinsic, Vec<Expression>),
    Structure(Vec<Expression>),
    Variant(VariantIndex, Vec<Expression>),
    Tuple(Vec<Expression>),
    Format(Vec<(InternedString, Expression)>, Option<InternedString>),
    Number(rust_decimal::Decimal),
    Constant(ItemId),
    With((ConstantId, Box<Expression>), Box<Expression>),
    ContextualConstant(ConstantId),
    End(Box<Expression>),
    Extend(Box<Expression>, BTreeMap<FieldIndex, Expression>),
}

#[derive(Debug, Clone)]
pub struct Arm {
    pub span: Option<SpanList>,
    pub pattern: Pattern,
    pub guard: Option<Expression>,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub span: Option<SpanList>,
    pub kind: PatternKind,
}

#[derive(Debug, Clone)]
pub enum PatternKind {
    Wildcard,
    Variable(VariableId),
    Text(InternedString),
    Number(rust_decimal::Decimal),
    Tuple(Vec<Pattern>),
    Destructure(BTreeMap<FieldIndex, Pattern>),
    Variant(VariantIndex, Vec<Pattern>),
    Or(Box<Pattern>, Box<Pattern>),
}

impl Compiler {
    pub(super) fn convert_to_ssa(&self, program: &analysis::Program) -> Program {
        let mut converter = Converter {
            compiler: self.clone(),
            program,
            structures: Default::default(),
            structure_ids: Default::default(),
            enumerations: Default::default(),
            enumeration_ids: Default::default(),
        };

        Program {
            items: program
                .items
                .iter()
                .map(|(&id, item)| {
                    let item = item.read();
                    let (_, item) = &*item;

                    (
                        id,
                        converter.convert_expr(
                            item,
                            program
                                .top_level
                                .map_or(true, |entrypoint| id != entrypoint),
                        ),
                    )
                })
                .collect(),
            contexts: program
                .contexts
                .iter()
                .map(|(id, item)| {
                    let decl = program.declarations.constants.get(id).unwrap();
                    (*id, (converter.convert_type(&decl.ty), *item))
                })
                .collect(),
            variables: program
                .declarations
                .variables
                .iter()
                .filter_map(|(var, decl)| {
                    if !decl.ty.params().is_empty() {
                        return None;
                    }

                    Some((*var, converter.convert_type(&decl.ty)))
                })
                .collect(),
            structures: converter.structures,
            enumerations: converter.enumerations,
            entrypoint: program.top_level.expect("no entrypoint provided"),
            entrypoint_wrapper: program.entrypoint_wrapper,
        }
    }
}

struct Converter<'a> {
    compiler: Compiler,
    program: &'a analysis::Program,
    structures: BTreeMap<StructureId, Vec<Type>>,
    structure_ids: BTreeMap<TypeId, StructureId>,
    enumerations: BTreeMap<EnumerationId, Vec<Vec<Type>>>,
    enumeration_ids: BTreeMap<TypeId, EnumerationId>,
}

impl Converter<'_> {
    fn convert_expr(&mut self, expr: &analysis::Expression, tail: bool) -> Expression {
        Expression {
            span: Some(expr.span),
            tail,
            ty: self.convert_type(&expr.ty),
            kind: match &expr.kind {
                analysis::ExpressionKind::Error(trace) => {
                    panic!("found error expression in program: {:?}", trace)
                }
                analysis::ExpressionKind::ErrorConstant(id) => {
                    panic!("found error constant in program: {:?}", id);
                }
                analysis::ExpressionKind::BoundInstance(_) => {
                    panic!("cannot lower a generic constant to IR: {:#?}", expr.span);
                }
                analysis::ExpressionKind::UnresolvedConstant(_)
                | analysis::ExpressionKind::UnresolvedTrait(_)
                | analysis::ExpressionKind::UnresolvedExtend => {
                    panic!("found unresolved expression in program: {:#?}", expr.span)
                }
                analysis::ExpressionKind::Marker => ExpressionKind::Marker,
                analysis::ExpressionKind::Variable(var) => ExpressionKind::Variable(*var),
                analysis::ExpressionKind::Text(text) => ExpressionKind::Text(*text),
                analysis::ExpressionKind::Block(exprs, _) => {
                    ExpressionKind::Block(self.convert_block(exprs, tail))
                }
                analysis::ExpressionKind::Call(func, input, _first) => ExpressionKind::Call(
                    Box::new(self.convert_expr(func, false)),
                    Box::new(self.convert_expr(input, false)),
                ),
                analysis::ExpressionKind::Function(pattern, body, captures) => {
                    ExpressionKind::Function(
                        self.convert_pattern(pattern),
                        Box::new(self.convert_expr(body, captures.is_empty())),
                        captures
                            .iter()
                            .map(|(var, ty)| (*var, self.convert_type(ty)))
                            .collect(),
                    )
                }
                analysis::ExpressionKind::When(input, arms) => ExpressionKind::When(
                    Box::new(self.convert_expr(input, false)),
                    arms.iter().map(|arm| self.convert_arm(arm, tail)).collect(),
                ),
                analysis::ExpressionKind::External(abi, identifier, inputs) => {
                    ExpressionKind::External(
                        *abi,
                        *identifier,
                        inputs
                            .iter()
                            .map(|expr| self.convert_expr(expr, false))
                            .collect(),
                    )
                }
                analysis::ExpressionKind::Intrinsic(intrinsic, inputs) => {
                    ExpressionKind::Intrinsic(
                        *intrinsic,
                        inputs
                            .iter()
                            .map(|expr| self.convert_expr(expr, false))
                            .collect(),
                    )
                }
                analysis::ExpressionKind::Initialize(_, _) => {
                    unreachable!(
                        "variable initialization is handled specially by convert_block_to_ssa"
                    )
                }
                analysis::ExpressionKind::Structure(exprs) => ExpressionKind::Structure(
                    exprs
                        .iter()
                        .map(|expr| self.convert_expr(expr, false))
                        .collect(),
                ),
                analysis::ExpressionKind::Variant(discriminant, exprs) => ExpressionKind::Variant(
                    *discriminant,
                    exprs
                        .iter()
                        .map(|expr| self.convert_expr(expr, false))
                        .collect(),
                ),
                analysis::ExpressionKind::Tuple(exprs) => ExpressionKind::Tuple(
                    exprs
                        .iter()
                        .map(|expr| self.convert_expr(expr, false))
                        .collect(),
                ),
                analysis::ExpressionKind::Format(segments, trailing_segment) => {
                    ExpressionKind::Format(
                        segments
                            .iter()
                            .map(|(text, expr)| (*text, self.convert_expr(expr, false)))
                            .collect(),
                        *trailing_segment,
                    )
                }
                analysis::ExpressionKind::Number(number) => ExpressionKind::Number(*number),
                analysis::ExpressionKind::Constant(constant)
                | analysis::ExpressionKind::ExpandedConstant(constant) => {
                    ExpressionKind::Constant(*constant)
                }
                analysis::ExpressionKind::With((id, value), body) => {
                    let id = id.expect("found error in `with` expression");

                    ExpressionKind::With(
                        (id, Box::new(self.convert_expr(value, false))),
                        Box::new(self.convert_expr(body, tail)),
                    )
                }
                analysis::ExpressionKind::ContextualConstant(id) => {
                    ExpressionKind::ContextualConstant(*id)
                }
                analysis::ExpressionKind::End(value) => {
                    ExpressionKind::End(Box::new(self.convert_expr(value, true)))
                }
                analysis::ExpressionKind::Extend(value, fields) => ExpressionKind::Extend(
                    Box::new(self.convert_expr(value, false)),
                    fields
                        .iter()
                        .map(|(&index, field)| (index, self.convert_expr(field, false)))
                        .collect(),
                ),
                analysis::ExpressionKind::Semantics(_semantics, expr) => {
                    // TODO: Handle semantics as necessary

                    return self.convert_expr(expr, tail);
                }
            },
        }
    }

    fn convert_block(&mut self, exprs: &[analysis::Expression], tail: bool) -> Vec<Expression> {
        let mut result = Vec::new();
        let count = exprs.len();
        for (index, expr) in exprs.iter().enumerate() {
            let tail = tail && index + 1 == count;

            if let analysis::ExpressionKind::Initialize(pattern, value) = &expr.kind {
                let remaining = &exprs[(index + 1)..];

                result.push(Expression {
                    span: Some(expr.span),
                    tail,
                    ty: self.convert_type(&expr.ty),
                    kind: ExpressionKind::When(
                        Box::new(self.convert_expr(value, false)),
                        vec![Arm {
                            span: Some(pattern.span),
                            pattern: self.convert_pattern(pattern),
                            guard: None,
                            body: Expression {
                                tail,
                                ty: remaining
                                    .last()
                                    .map(|expr| self.convert_type(&expr.ty))
                                    .unwrap_or_else(|| Type::Tuple(Vec::new())),
                                span: remaining.first().map(|expr| {
                                    SpanList::join(expr.span, remaining.last().unwrap().span)
                                }),
                                kind: ExpressionKind::Block(self.convert_block(remaining, tail)),
                            },
                        }],
                    ),
                });

                break;
            } else {
                result.push(self.convert_expr(expr, tail));
            }
        }

        result
    }

    fn convert_arm(&mut self, arm: &analysis::Arm, tail: bool) -> Arm {
        Arm {
            span: Some(arm.span),
            pattern: self.convert_pattern(&arm.pattern),
            guard: arm
                .guard
                .as_ref()
                .map(|expr| self.convert_expr(expr, false)),
            body: self.convert_expr(&arm.body, tail),
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn convert_pattern(&mut self, pattern: &analysis::Pattern) -> Pattern {
        Pattern {
            span: Some(pattern.span),
            kind: match &pattern.kind {
                analysis::PatternKind::Error(trace) => {
                    panic!("found error pattern in program: {trace:?}");
                }
                analysis::PatternKind::UnresolvedDestructure
                | analysis::PatternKind::UnresolvedVariant => {
                    panic!("found unresolved pattern in program")
                }
                analysis::PatternKind::Wildcard => PatternKind::Wildcard,
                analysis::PatternKind::Variable(var) => PatternKind::Variable(*var),
                analysis::PatternKind::Text(text) => PatternKind::Text(*text),
                analysis::PatternKind::Number(number) => PatternKind::Number(*number),
                analysis::PatternKind::Tuple(patterns) => PatternKind::Tuple(
                    patterns
                        .iter()
                        .map(|pattern| self.convert_pattern(pattern))
                        .collect(),
                ),
                analysis::PatternKind::Destructure(_, fields) => PatternKind::Destructure(
                    fields
                        .iter()
                        .map(|(index, pattern)| (*index, self.convert_pattern(pattern)))
                        .collect(),
                ),
                analysis::PatternKind::Variant(_, discriminant, values) => PatternKind::Variant(
                    *discriminant,
                    values
                        .iter()
                        .map(|pattern| self.convert_pattern(pattern))
                        .collect(),
                ),
                analysis::PatternKind::Or(left, right) => PatternKind::Or(
                    Box::new(self.convert_pattern(left)),
                    Box::new(self.convert_pattern(right)),
                ),
            },
        }
    }

    fn convert_type(&mut self, ty: &analysis::Type) -> Type {
        match &ty.kind {
            analysis::TypeKind::Parameter(id) => panic!("unexpected type parameter {id:#?}"),
            analysis::TypeKind::Named(id, _, structure) => match structure {
                analysis::TypeStructure::Marker => Type::Marker,
                analysis::TypeStructure::Structure(tys) => {
                    let structure_id = self.compiler.new_structure_id();
                    self.structure_ids.insert(*id, structure_id);

                    let tys = tys.iter().map(|ty| self.convert_type(ty)).collect();
                    self.structures.insert(structure_id, tys);

                    Type::Structure(structure_id)
                }
                analysis::TypeStructure::Enumeration(variants) => {
                    let enumeration_id = self.compiler.new_enumeration_id();
                    self.enumeration_ids.insert(*id, enumeration_id);

                    let variants = variants
                        .iter()
                        .map(|tys| tys.iter().map(|ty| self.convert_type(ty)).collect())
                        .collect();
                    self.enumerations.insert(enumeration_id, variants);

                    Type::Enumeration(enumeration_id)
                }
                analysis::TypeStructure::Recursive(id) => {
                    match &self.program.declarations.types.get(id).unwrap().kind {
                        analysis::typecheck::TypeDeclKind::Marker => {
                            unreachable!(
                                "marker types cannot be recursive because they store no data"
                            )
                        }
                        analysis::typecheck::TypeDeclKind::Structure { .. } => {
                            Type::StructureReference(*self.structure_ids.get(id).unwrap())
                        }
                        analysis::typecheck::TypeDeclKind::Enumeration { .. } => {
                            Type::EnumerationReference(*self.enumeration_ids.get(id).unwrap())
                        }
                        analysis::typecheck::TypeDeclKind::Alias(ty) => self.convert_type(ty),
                    }
                }
            },
            analysis::TypeKind::Function(input, output) => Type::FunctionReference(
                Box::new(self.convert_type(input)),
                Box::new(self.convert_type(output)),
            ),
            analysis::TypeKind::Tuple(tys) => {
                Type::Tuple(tys.iter().map(|ty| self.convert_type(ty)).collect())
            }
            analysis::TypeKind::Builtin(ty) => match ty {
                analysis::typecheck::BuiltinType::Number => Type::Number,
                analysis::typecheck::BuiltinType::Text => Type::TextReference,
                analysis::typecheck::BuiltinType::List(ty) => {
                    Type::ListReference(Box::new(self.convert_type(ty)))
                }
                analysis::typecheck::BuiltinType::Reference(ty) => {
                    Type::MutableReference(Box::new(self.convert_type(ty)))
                }
                analysis::typecheck::BuiltinType::Ui => Type::Ui,
                analysis::typecheck::BuiltinType::TaskGroup => Type::TaskGroup,
                analysis::typecheck::BuiltinType::Hasher => Type::Hasher,
            },
            analysis::TypeKind::Error => panic!("unexpected error type: {:#?}", ty.info),
        }
    }
}
