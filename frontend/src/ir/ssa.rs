use crate::{
    analysis, helpers::InternedString, parse::Span, Compiler, EnumerationId, FieldIndex, ItemId,
    StructureId, TypeId, VariableId, VariantIndex,
};
use std::{
    collections::BTreeMap,
    os::raw::{c_int, c_uint},
};

pub use crate::analysis::RuntimeFunction;

#[derive(Debug, Clone)]
pub struct Program {
    pub items: BTreeMap<ItemId, Expression>,
    pub structures: BTreeMap<StructureId, Vec<Type>>,
    pub enumerations: BTreeMap<EnumerationId, Vec<Vec<Type>>>,
    pub entrypoint: ItemId,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub span: Option<Span>,
    pub tail: bool,
    pub ty: Type,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone)]
pub enum Type {
    Marker,
    Number,
    Integer,
    Natural,
    Byte,
    Signed,
    Unsigned,
    Float,
    Double,
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
    End(Box<Expression>),
    Call(Box<Expression>, Box<Expression>),
    Function(Pattern, Box<Expression>, analysis::lower::CaptureList),
    When(Box<Expression>, Vec<Arm>),
    External(InternedString, InternedString, Vec<Expression>),
    Runtime(RuntimeFunction, Vec<Expression>),
    Structure(Vec<Expression>),
    Variant(VariantIndex, Vec<Expression>),
    Tuple(Vec<Expression>),
    Number(rust_decimal::Decimal),
    Integer(i64),
    Natural(u64),
    Byte(u8),
    Signed(c_int),
    Unsigned(c_uint),
    Float(f32),
    Double(f64),
    Constant(ItemId),
}

#[derive(Debug, Clone)]
pub struct Arm {
    pub span: Option<Span>,
    pub pattern: Pattern,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub span: Option<Span>,
    pub kind: PatternKind,
}

#[derive(Debug, Clone)]
pub enum PatternKind {
    Wildcard,
    Variable(VariableId),
    Text(InternedString),
    Number(rust_decimal::Decimal),
    Integer(i64),
    Natural(u64),
    Byte(u8),
    Signed(c_int),
    Unsigned(c_uint),
    Float(f32),
    Double(f64),
    Tuple(Vec<Pattern>),
    Destructure(BTreeMap<FieldIndex, Pattern>),
    Variant(VariantIndex, Vec<Pattern>),
    Or(Box<Pattern>, Box<Pattern>),
    Where(Box<Pattern>, Box<Expression>),
}

impl Compiler<'_> {
    pub(super) fn convert_to_ssa(&self, program: &analysis::Program) -> Program {
        let mut converter = Converter {
            compiler: self,
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
                .map(|(id, (_, item))| (*id, converter.convert_expr(item, true)))
                .collect(),
            structures: converter.structures,
            enumerations: converter.enumerations,
            entrypoint: program.entrypoint.expect("no entrypoint provided"),
        }
    }
}

impl Type {
    pub fn is_reference(&self) -> bool {
        match self {
            Type::Marker
            | Type::Number
            | Type::Integer
            | Type::Natural
            | Type::Byte
            | Type::Signed
            | Type::Unsigned
            | Type::Float
            | Type::Double
            | Type::Tuple(_)
            | Type::Structure(_)
            | Type::Enumeration(_) => false,
            Type::TextReference
            | Type::ListReference(_)
            | Type::MutableReference(_)
            | Type::FunctionReference(_, _)
            | Type::StructureReference(_)
            | Type::EnumerationReference(_) => true,
        }
    }
}

struct Converter<'a, 'l> {
    compiler: &'a Compiler<'l>,
    program: &'a analysis::Program,
    structures: BTreeMap<StructureId, Vec<Type>>,
    structure_ids: BTreeMap<TypeId, StructureId>,
    enumerations: BTreeMap<EnumerationId, Vec<Vec<Type>>>,
    enumeration_ids: BTreeMap<TypeId, EnumerationId>,
}

impl Converter<'_, '_> {
    fn convert_expr(&mut self, expr: &analysis::Expression, tail: bool) -> Expression {
        Expression {
            span: Some(expr.span),
            tail,
            ty: self.convert_type(&expr.ty),
            kind: match &expr.kind {
                analysis::ExpressionKind::Error(trace) => {
                    panic!(
                        "found error expression in program: {:?}",
                        trace.clone().into_inner()
                    )
                }
                analysis::ExpressionKind::Marker => ExpressionKind::Marker,
                analysis::ExpressionKind::Variable(var) => ExpressionKind::Variable(*var),
                analysis::ExpressionKind::Text(text) => ExpressionKind::Text(*text),
                analysis::ExpressionKind::Block(exprs, _) => {
                    ExpressionKind::Block(self.convert_block(exprs, tail))
                }
                analysis::ExpressionKind::End(value) => {
                    ExpressionKind::End(Box::new(self.convert_expr(value, tail)))
                }
                analysis::ExpressionKind::Call(func, input) => ExpressionKind::Call(
                    Box::new(self.convert_expr(func, false)),
                    Box::new(self.convert_expr(input, false)),
                ),
                analysis::ExpressionKind::Function(pattern, body, captures) => {
                    ExpressionKind::Function(
                        self.convert_pattern(pattern),
                        Box::new(self.convert_expr(body, tail)),
                        captures.clone(),
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
                analysis::ExpressionKind::Runtime(func, inputs) => ExpressionKind::Runtime(
                    *func,
                    inputs
                        .iter()
                        .map(|expr| self.convert_expr(expr, false))
                        .collect(),
                ),
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
                analysis::ExpressionKind::Number(number) => ExpressionKind::Number(*number),
                analysis::ExpressionKind::Integer(integer) => ExpressionKind::Integer(*integer),
                analysis::ExpressionKind::Natural(natural) => ExpressionKind::Natural(*natural),
                analysis::ExpressionKind::Byte(byte) => ExpressionKind::Byte(*byte),
                analysis::ExpressionKind::Signed(signed) => ExpressionKind::Signed(*signed),
                analysis::ExpressionKind::Unsigned(unsigned) => ExpressionKind::Unsigned(*unsigned),
                analysis::ExpressionKind::Float(float) => ExpressionKind::Float(*float),
                analysis::ExpressionKind::Double(double) => ExpressionKind::Double(*double),
                analysis::ExpressionKind::Constant(constant)
                | analysis::ExpressionKind::ExpandedConstant(constant) => {
                    ExpressionKind::Constant(*constant)
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
                            body: Expression {
                                tail,
                                ty: remaining
                                    .last()
                                    .map(|expr| self.convert_type(&expr.ty))
                                    .unwrap_or_else(|| Type::Tuple(Vec::new())),
                                span: remaining.first().map(|expr| {
                                    expr.span.with_end(remaining.last().unwrap().span.end)
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
            body: self.convert_expr(&arm.body, tail),
        }
    }

    fn convert_pattern(&mut self, pattern: &analysis::Pattern) -> Pattern {
        Pattern {
            span: Some(pattern.span),
            kind: match &pattern.kind {
                analysis::PatternKind::Error(trace) => {
                    panic!("found error pattern in program: {:?}", trace);
                }
                analysis::PatternKind::Wildcard => PatternKind::Wildcard,
                analysis::PatternKind::Variable(var) => PatternKind::Variable(*var),
                analysis::PatternKind::Text(text) => PatternKind::Text(*text),
                analysis::PatternKind::Number(number) => PatternKind::Number(*number),
                analysis::PatternKind::Integer(integer) => PatternKind::Integer(*integer),
                analysis::PatternKind::Natural(natural) => PatternKind::Natural(*natural),
                analysis::PatternKind::Byte(byte) => PatternKind::Byte(*byte),
                analysis::PatternKind::Signed(signed) => PatternKind::Signed(*signed),
                analysis::PatternKind::Unsigned(unsigned) => PatternKind::Unsigned(*unsigned),
                analysis::PatternKind::Float(float) => PatternKind::Float(*float),
                analysis::PatternKind::Double(double) => PatternKind::Double(*double),
                analysis::PatternKind::Tuple(patterns) => PatternKind::Tuple(
                    patterns
                        .iter()
                        .map(|pattern| self.convert_pattern(pattern))
                        .collect(),
                ),
                analysis::PatternKind::Destructure(fields) => PatternKind::Destructure(
                    fields
                        .iter()
                        .map(|(index, pattern)| (*index, self.convert_pattern(pattern)))
                        .collect(),
                ),
                analysis::PatternKind::Variant(discriminant, values) => PatternKind::Variant(
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
                analysis::PatternKind::Where(pattern, condition) => PatternKind::Where(
                    Box::new(self.convert_pattern(pattern)),
                    Box::new(self.convert_expr(condition, false)),
                ),
            },
        }
    }

    fn convert_type(&mut self, ty: &analysis::Type) -> Type {
        match ty {
            analysis::Type::Parameter(_) => panic!("unexpected type parameter"),
            analysis::Type::Named(id, _, structure) => match structure {
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
                    }
                }
            },
            analysis::Type::Function(input, output) => Type::FunctionReference(
                Box::new(self.convert_type(input)),
                Box::new(self.convert_type(output)),
            ),
            analysis::Type::Tuple(tys) => {
                Type::Tuple(tys.iter().map(|ty| self.convert_type(ty)).collect())
            }
            analysis::Type::Builtin(ty) => match ty {
                analysis::typecheck::BuiltinType::Number => Type::Number,
                analysis::typecheck::BuiltinType::Integer => Type::Integer,
                analysis::typecheck::BuiltinType::Natural => Type::Natural,
                analysis::typecheck::BuiltinType::Byte => Type::Byte,
                analysis::typecheck::BuiltinType::Signed => Type::Signed,
                analysis::typecheck::BuiltinType::Unsigned => Type::Unsigned,
                analysis::typecheck::BuiltinType::Float => Type::Float,
                analysis::typecheck::BuiltinType::Double => Type::Double,
                analysis::typecheck::BuiltinType::Text => Type::TextReference,
                analysis::typecheck::BuiltinType::List(ty) => {
                    Type::ListReference(Box::new(self.convert_type(ty)))
                }
                analysis::typecheck::BuiltinType::Mutable(ty) => {
                    Type::MutableReference(Box::new(self.convert_type(ty)))
                }
            },
            analysis::Type::Error => unreachable!(),
        }
    }
}
