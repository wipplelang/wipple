#![allow(clippy::too_many_arguments, clippy::new_without_default)]

mod format;

use crate::{
    analysis::typecheck, helpers::InternedString, Compiler, ComputationId, Loader,
    MonomorphizedConstantId, TypeId, VariableId,
};
use std::{collections::BTreeMap, mem};

pub mod lib {
    pub const RUNTIME: &str = "runtime";
}

#[derive(Debug, Clone)]
pub struct Program {
    pub nominal_types: BTreeMap<TypeId, Type>,
    pub constants: BTreeMap<MonomorphizedConstantId, Constant>,
    pub entrypoint: Sections,
}

#[derive(Debug, Clone)]
pub struct Section<I = SectionIndex> {
    pub statements: Vec<Statement<I>>,
    pub terminator: Option<Terminator<I>>,
}

impl<I> Section<I> {
    pub fn new() -> Self {
        Section {
            statements: Vec::new(),
            terminator: None,
        }
    }

    fn add_statement(&mut self, statement: Statement<I>) {
        assert!(!self.has_terminator());
        self.statements.push(statement);
    }

    fn set_terminator(&mut self, terminator: Terminator<I>) {
        assert!(!self.has_terminator());
        self.terminator = Some(terminator);
    }

    fn has_terminator(&self) -> bool {
        self.terminator.is_some()
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Number,
    Text,
    Integer,
    Discriminant,
    Boolean,
    Function(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
    List(Box<Type>),
    Mutable(Box<Type>),
    Marker,
    Structure(TypeId, Vec<Type>),
    Enumeration(TypeId, Vec<Vec<Type>>),
    Unreachable,
}

#[derive(Debug, Clone)]
pub struct Expression<I = SectionIndex> {
    pub ty: Type,
    pub kind: ExpressionKind<I>,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind<I = SectionIndex> {
    Marker,
    Constant(MonomorphizedConstantId),
    Function(Function<I>),
    Variable(VariableId),
    FunctionInput,
    Number(f64),
    Text(InternedString),
    Call(ComputationId, ComputationId),
    External(InternedString, InternedString, Vec<ComputationId>),
    Tuple(Vec<ComputationId>),
    Structure(Vec<ComputationId>),
    Variant(usize, Vec<ComputationId>),
    TupleElement(ComputationId, usize),
    StructureElement(ComputationId, TypeId, usize),
    VariantElement(ComputationId, (TypeId, usize), usize),
    Discriminant(ComputationId),
    CompareDiscriminants(ComputationId, usize),
}

#[derive(Debug, Clone)]
pub enum Statement<I = SectionIndex> {
    Compute(ComputationId, Expression<I>),
    Initialize(VariableId, ComputationId),
    Drop(Vec<VariableId>),
}

#[derive(Debug, Clone)]
pub enum Terminator<I = SectionIndex> {
    If(ComputationId, I, I),
    Return(ComputationId),
    Goto(I),
    Unreachable,
}

#[derive(Debug, Clone)]
pub struct Constant<I = SectionIndex> {
    pub ty: Type,
    pub sections: Sections<I>,
}

#[derive(Debug, Clone)]
pub struct Function<I = SectionIndex> {
    pub captures: Vec<VariableId>,
    pub locals: Vec<VariableId>,
    pub sections: Sections<I>,
}

#[derive(Debug, Clone)]
pub struct Sections<I = SectionIndex>(Vec<Section<I>>);

impl<I> Default for Sections<I> {
    fn default() -> Self {
        Sections(vec![Section::new()])
    }
}

impl<I> Sections<I> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn enumerate(&self) -> impl Iterator<Item = (SectionIndex, &Section<I>)> {
        self.0
            .iter()
            .enumerate()
            .map(|(index, section)| (SectionIndex(index), section))
    }

    fn current_index(&self) -> SectionIndex {
        SectionIndex(self.next_index().0 - 1)
    }

    fn next_index(&self) -> SectionIndex {
        SectionIndex(self.0.len())
    }

    fn add_section(&mut self) -> SectionIndex {
        self.add_section_with(Section::new())
    }

    fn add_section_with(&mut self, section: Section<I>) -> SectionIndex {
        let index = self.next_index();
        self.0.push(section);
        index
    }

    fn add_statement(&mut self, statement: Statement<I>) {
        self.0.last_mut().unwrap().add_statement(statement);
    }

    pub fn set_terminator(&mut self, terminator: Terminator<I>) {
        self.0.last_mut().unwrap().set_terminator(terminator);
    }

    fn has_terminator(&self) -> bool {
        self.0.last().unwrap().has_terminator()
    }
}

impl<S> std::ops::Index<SectionIndex> for Sections<S> {
    type Output = Section<S>;

    fn index(&self, index: SectionIndex) -> &Self::Output {
        &self.0[index.0]
    }
}

impl<S> std::ops::IndexMut<SectionIndex> for Sections<S> {
    fn index_mut(&mut self, index: SectionIndex) -> &mut Self::Output {
        &mut self.0[index.0]
    }
}

impl<S> FromIterator<Section<S>> for Sections<S> {
    fn from_iter<T: IntoIterator<Item = Section<S>>>(iter: T) -> Self {
        Sections(Vec::from_iter(iter))
    }
}

impl<S> IntoIterator for Sections<S> {
    type Item = Section<S>;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SectionIndex(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum UnresolvedSectionIndex {
    Resolved(SectionIndex),
    Unresolved(usize),
}

impl<L: Loader> Compiler<L> {
    pub fn ir_from(&mut self, program: &typecheck::Program) -> Program {
        let mut info = Info::default();

        let mut constants = BTreeMap::new();
        for (id, ((), _, constant)) in program.declarations.monomorphized_constants.iter().rev() {
            let ty = self.gen_type(constant.value.ty.clone(), &mut info);

            let mut sections = Sections::new();

            if let Some(result) =
                self.gen_computation_from_expr(&constant.value, None, &mut sections, &mut info)
            {
                sections.set_terminator(Terminator::Return(result))
            }

            constants.insert(*id, Constant { ty, sections });
        }

        let mut entrypoint = Sections::new();
        for expr in &program.body {
            self.gen_computation_from_expr(expr, None, &mut entrypoint, &mut info);
        }

        let map = info.map;

        Program {
            nominal_types: info.nominal_types,
            constants: constants
                .into_iter()
                .map(|(id, constant)| (id, constant.finalize(&map)))
                .collect(),
            entrypoint: entrypoint.finalize(&map),
        }
    }
}

type SectionIndexMap = BTreeMap<usize, Option<SectionIndex>>;

#[derive(Default)]
struct Info {
    nominal_types: BTreeMap<TypeId, Type>,
    map: SectionIndexMap,
    function: Vec<(Vec<VariableId>, Vec<VariableId>)>,
    block: Vec<VariableId>,
}

impl Info {
    fn with_unresolved_section_index(
        &mut self,
        f: impl FnOnce(&mut Self, UnresolvedSectionIndex) -> SectionIndex,
    ) -> SectionIndex {
        let unresolved = self.map.len();
        self.map.insert(unresolved, None);
        let index = f(self, UnresolvedSectionIndex::Unresolved(unresolved));
        self.map.insert(unresolved, Some(index));
        index
    }
}

impl<L: Loader> Compiler<L> {
    fn gen_computation_from_expr(
        &mut self,
        expr: &typecheck::Expression,
        loop_info: Option<(UnresolvedSectionIndex, UnresolvedSectionIndex)>,
        sections: &mut Sections<UnresolvedSectionIndex>,
        info: &mut Info,
    ) -> Option<ComputationId> {
        let id = self.new_computation_id();
        self.gen_computation_from_expr_with(id, expr, loop_info, sections, info)?;
        Some(id)
    }

    fn gen_computation_from_expr_with(
        &mut self,
        computation: ComputationId,
        expr: &typecheck::Expression,
        loop_info: Option<(UnresolvedSectionIndex, UnresolvedSectionIndex)>,
        sections: &mut Sections<UnresolvedSectionIndex>,
        info: &mut Info,
    ) -> Option<()> {
        let ty = self.gen_type(expr.ty.clone(), info);

        {
            let ty = ty.clone();

            (|| {
                macro_rules! or_unreachable {
                    ($computation:expr) => {
                        match $computation {
                            Some(computation) => computation,
                            None => return,
                        }
                    };
                }

                match &expr.kind {
                    typecheck::ExpressionKind::Marker => {
                        sections.add_statement(Statement::Compute(
                            computation,
                            Expression {
                                ty,
                                kind: ExpressionKind::Marker,
                            },
                        ));
                    }
                    typecheck::ExpressionKind::Variable(var) => {
                        for (locals, captures) in info.function.iter_mut().rev() {
                            if locals.contains(var) {
                                break;
                            }

                            captures.push(*var);
                        }

                        sections.add_statement(Statement::Compute(
                            computation,
                            Expression {
                                ty,
                                kind: ExpressionKind::Variable(*var),
                            },
                        ));
                    }
                    typecheck::ExpressionKind::Text(text) => {
                        sections.add_statement(Statement::Compute(
                            computation,
                            Expression {
                                ty,
                                kind: ExpressionKind::Text(*text),
                            },
                        ));
                    }
                    typecheck::ExpressionKind::Number(number) => {
                        sections.add_statement(Statement::Compute(
                            computation,
                            Expression {
                                ty,
                                kind: ExpressionKind::Number(*number),
                            },
                        ));
                    }
                    typecheck::ExpressionKind::Block(exprs) => {
                        let prev_block = mem::take(&mut info.block);

                        let count = exprs.len();

                        if exprs.is_empty() {
                            sections.add_statement(Statement::Compute(
                                computation,
                                Expression {
                                    ty,
                                    kind: ExpressionKind::Tuple(Vec::new()),
                                },
                            ));
                        } else {
                            for (index, expr) in exprs.iter().enumerate() {
                                if index + 1 == count {
                                    self.gen_computation_from_expr_with(
                                        computation,
                                        expr,
                                        loop_info,
                                        sections,
                                        info,
                                    );
                                } else {
                                    self.gen_computation_from_expr(expr, loop_info, sections, info);
                                }
                            }
                        }

                        let block = mem::replace(&mut info.block, prev_block);

                        if !block.is_empty() {
                            sections
                                .add_statement(Statement::Drop(block.into_iter().rev().collect()));
                        }
                    }
                    typecheck::ExpressionKind::Call(function, input) => {
                        let function = or_unreachable!(
                            self.gen_computation_from_expr(function, loop_info, sections, info)
                        );

                        let input = or_unreachable!(
                            self.gen_computation_from_expr(input, loop_info, sections, info)
                        );

                        sections.add_statement(Statement::Compute(
                            computation,
                            Expression {
                                ty,
                                kind: ExpressionKind::Call(function, input),
                            },
                        ));
                    }
                    typecheck::ExpressionKind::Function(pattern, body) => {
                        let input_ty = match &ty {
                            Type::Function(input, _) => (**input).clone(),
                            _ => unreachable!(),
                        };

                        let prev_block = mem::take(&mut info.block);
                        let prev_sections = mem::take(sections);
                        info.function.push(Default::default());

                        let input = self.new_computation_id();
                        sections.add_statement(Statement::Compute(
                            input,
                            Expression {
                                ty: input_ty.clone(),
                                kind: ExpressionKind::FunctionInput,
                            },
                        ));

                        self.gen_sections_from_init(
                            pattern,
                            input,
                            &input_ty,
                            computation,
                            None,
                            sections,
                            info,
                        );

                        let result = self.gen_computation_from_expr(body, None, sections, info);

                        let (locals, captures) = info.function.pop().unwrap();

                        let mut function_sections = mem::replace(sections, prev_sections);
                        info.block = prev_block;

                        let captures = info
                            .function
                            .iter()
                            .flat_map(|(_, captures)| captures)
                            .copied()
                            .chain(captures)
                            .collect::<Vec<_>>();

                        if !captures.is_empty() {
                            function_sections.add_statement(Statement::Drop(
                                captures.iter().copied().rev().collect(),
                            ));
                        }

                        if let Some(result) = result {
                            function_sections.set_terminator(Terminator::Return(result));
                        }

                        let function = Function {
                            captures,
                            locals,
                            sections: function_sections,
                        };

                        sections.add_statement(Statement::Compute(
                            computation,
                            Expression {
                                ty,
                                kind: ExpressionKind::Function(function),
                            },
                        ));
                    }
                    typecheck::ExpressionKind::When(input, arms) => {
                        let input_ty = self.gen_type(input.ty.clone(), info);
                        let input = or_unreachable!(
                            self.gen_computation_from_expr(input, loop_info, sections, info)
                        );

                        self.gen_sections_from_when(
                            arms,
                            input,
                            &input_ty,
                            computation,
                            loop_info,
                            sections,
                            info,
                        );
                    }
                    typecheck::ExpressionKind::External(lib, identifier, exprs) => {
                        let mut inputs = Vec::with_capacity(exprs.len());
                        for expr in exprs {
                            let computation = or_unreachable!(
                                self.gen_computation_from_expr(expr, loop_info, sections, info)
                            );

                            inputs.push(computation);
                        }

                        sections.add_statement(Statement::Compute(
                            computation,
                            Expression {
                                ty,
                                kind: ExpressionKind::External(*lib, *identifier, inputs),
                            },
                        ));
                    }
                    typecheck::ExpressionKind::Initialize(pattern, value) => {
                        let value_ty = self.gen_type(value.ty.clone(), info);
                        let value = or_unreachable!(
                            self.gen_computation_from_expr(value, loop_info, sections, info)
                        );

                        self.gen_sections_from_init(
                            pattern,
                            value,
                            &value_ty,
                            computation,
                            loop_info,
                            sections,
                            info,
                        );

                        sections.add_statement(Statement::Compute(
                            computation,
                            Expression {
                                ty,
                                kind: ExpressionKind::Tuple(Vec::new()),
                            },
                        ));
                    }
                    typecheck::ExpressionKind::Structure(exprs) => {
                        let mut values = Vec::with_capacity(exprs.len());
                        for expr in exprs {
                            let computation = or_unreachable!(
                                self.gen_computation_from_expr(expr, loop_info, sections, info)
                            );

                            values.push(computation);
                        }

                        sections.add_statement(Statement::Compute(
                            computation,
                            Expression {
                                ty,
                                kind: ExpressionKind::Structure(values),
                            },
                        ));
                    }
                    typecheck::ExpressionKind::Tuple(exprs) => {
                        let mut values = Vec::with_capacity(exprs.len());
                        for expr in exprs {
                            let computation = or_unreachable!(
                                self.gen_computation_from_expr(expr, loop_info, sections, info)
                            );

                            values.push(computation);
                        }

                        sections.add_statement(Statement::Compute(
                            computation,
                            Expression {
                                ty,
                                kind: ExpressionKind::Tuple(values),
                            },
                        ));
                    }
                    typecheck::ExpressionKind::Variant(discriminant, exprs) => {
                        let mut values = Vec::with_capacity(exprs.len());
                        for expr in exprs {
                            let computation = or_unreachable!(
                                self.gen_computation_from_expr(expr, loop_info, sections, info)
                            );

                            values.push(computation);
                        }
                        sections.add_statement(Statement::Compute(
                            computation,
                            Expression {
                                ty,
                                kind: ExpressionKind::Variant(*discriminant, values),
                            },
                        ));
                    }
                    typecheck::ExpressionKind::Return(expr) => {
                        let result = or_unreachable!(
                            self.gen_computation_from_expr(expr, loop_info, sections, info)
                        );

                        sections.set_terminator(Terminator::Return(result));
                        sections.add_section();
                    }
                    typecheck::ExpressionKind::Loop(body) => {
                        let loop_start = UnresolvedSectionIndex::Resolved(sections.current_index());

                        info.with_unresolved_section_index(|info, loop_end| {
                            self.gen_computation_from_expr_with(
                                computation,
                                body,
                                Some((loop_start, loop_end)),
                                sections,
                                info,
                            );

                            sections.set_terminator(Terminator::Goto(loop_start));

                            sections.add_section()
                        });
                    }
                    typecheck::ExpressionKind::Break(expr) => {
                        self.gen_computation_from_expr_with(
                            computation,
                            expr,
                            loop_info,
                            sections,
                            info,
                        );
                        sections.set_terminator(Terminator::Goto(loop_info.unwrap().1));
                        sections.add_section();
                    }
                    typecheck::ExpressionKind::Continue => {
                        sections.set_terminator(Terminator::Goto(loop_info.unwrap().0));
                        sections.add_section();
                    }
                    typecheck::ExpressionKind::Constant(id) => {
                        sections.add_statement(Statement::Compute(
                            computation,
                            Expression {
                                ty,
                                kind: ExpressionKind::Constant(*id),
                            },
                        ));
                    }
                }
            })();

            assert!(!sections.has_terminator());
        }

        (!matches!(ty, Type::Unreachable)).then(|| ())
    }

    fn gen_sections_from_init(
        &mut self,
        pattern: &typecheck::Pattern,
        input: ComputationId,
        input_ty: &Type,
        result: ComputationId,
        loop_info: Option<(UnresolvedSectionIndex, UnresolvedSectionIndex)>,
        sections: &mut Sections<UnresolvedSectionIndex>,
        info: &mut Info,
    ) {
        let span = pattern.span;

        let body = typecheck::Expression {
            span,
            ty: typecheck::Type::Tuple(Vec::new()),
            kind: typecheck::ExpressionKind::Tuple(Vec::new()),
        };

        info.with_unresolved_section_index(|info, end| {
            info.with_unresolved_section_index(|info, else_branch| {
                self.gen_sections_from_pattern(
                    pattern,
                    Err((&body, end)),
                    input_ty,
                    input,
                    result,
                    else_branch,
                    loop_info,
                    sections,
                    info,
                );

                sections.add_section()
            });

            sections.set_terminator(Terminator::Unreachable);

            sections.add_section()
        });
    }

    fn gen_sections_from_when(
        &mut self,
        arms: &[typecheck::Arm],
        input: ComputationId,
        input_ty: &Type,
        result: ComputationId,
        loop_info: Option<(UnresolvedSectionIndex, UnresolvedSectionIndex)>,
        sections: &mut Sections<UnresolvedSectionIndex>,
        info: &mut Info,
    ) {
        info.with_unresolved_section_index(|info, end| {
            for arm in arms {
                info.with_unresolved_section_index(|info, else_branch| {
                    self.gen_sections_from_pattern(
                        &arm.pattern,
                        Err((&arm.body, end)),
                        input_ty,
                        input,
                        result,
                        else_branch,
                        loop_info,
                        sections,
                        info,
                    );

                    sections.add_section()
                });
            }

            sections.set_terminator(Terminator::Unreachable);

            sections.add_section()
        });
    }

    fn gen_sections_from_pattern(
        &mut self,
        pattern: &typecheck::Pattern,
        body: Result<UnresolvedSectionIndex, (&typecheck::Expression, UnresolvedSectionIndex)>,
        input_ty: &Type,
        input: ComputationId,
        result: ComputationId,
        else_branch: UnresolvedSectionIndex,
        loop_info: Option<(UnresolvedSectionIndex, UnresolvedSectionIndex)>,
        sections: &mut Sections<UnresolvedSectionIndex>,
        info: &mut Info,
    ) {
        macro_rules! gen_then_branch {
            ($info:ident) => {
                match body {
                    Ok(section) => sections.set_terminator(Terminator::Goto(section)),
                    Err((expr, end)) => {
                        self.gen_computation_from_expr_with(
                            result, expr, loop_info, sections, $info,
                        );

                        sections.set_terminator(Terminator::Goto(end));
                    }
                }
            };
        }

        match &pattern.kind {
            typecheck::PatternKind::Wildcard => gen_then_branch!(info),
            typecheck::PatternKind::Number(number) => {
                let predicate = self.new_computation_id();
                sections.add_statement(Statement::Compute(
                    predicate,
                    Expression {
                        ty: Type::Number,
                        kind: ExpressionKind::Number(*number),
                    },
                ));

                let condition = self.new_computation_id();
                sections.add_statement(Statement::Compute(
                    condition,
                    Expression {
                        ty: Type::Boolean,
                        kind: ExpressionKind::External(
                            InternedString::new(lib::RUNTIME),
                            InternedString::new("number-equality"),
                            vec![input, predicate],
                        ),
                    },
                ));

                info.with_unresolved_section_index(|_, then_branch| {
                    sections.set_terminator(Terminator::If(condition, then_branch, else_branch));
                    sections.add_section()
                });

                gen_then_branch!(info);
            }
            typecheck::PatternKind::Text(text) => {
                let predicate = self.new_computation_id();
                sections.add_statement(Statement::Compute(
                    predicate,
                    Expression {
                        ty: Type::Text,
                        kind: ExpressionKind::Text(*text),
                    },
                ));

                let condition = self.new_computation_id();
                sections.add_statement(Statement::Compute(
                    condition,
                    Expression {
                        ty: Type::Boolean,
                        kind: ExpressionKind::External(
                            InternedString::new(lib::RUNTIME),
                            InternedString::new("text-equality"),
                            vec![input, predicate],
                        ),
                    },
                ));

                info.with_unresolved_section_index(|_, then_branch| {
                    sections.set_terminator(Terminator::If(condition, then_branch, else_branch));
                    sections.add_section()
                });

                gen_then_branch!(info);
            }
            typecheck::PatternKind::Variable(var) => {
                info.block.push(*var);
                if let Some((locals, _)) = info.function.last_mut() {
                    locals.push(*var);
                }

                sections.add_statement(Statement::Initialize(*var, input));
                gen_then_branch!(info);
            }
            typecheck::PatternKind::Or(left, right) => {
                info.with_unresolved_section_index(|info, then_branch| {
                    info.with_unresolved_section_index(|info, else_if_branch| {
                        self.gen_sections_from_pattern(
                            left,
                            Ok(then_branch),
                            input_ty,
                            input,
                            result,
                            else_if_branch,
                            loop_info,
                            sections,
                            info,
                        );

                        sections.add_section()
                    });

                    self.gen_sections_from_pattern(
                        right,
                        Ok(then_branch),
                        input_ty,
                        input,
                        result,
                        else_branch,
                        loop_info,
                        sections,
                        info,
                    );

                    sections.add_section()
                });

                gen_then_branch!(info);
            }
            typecheck::PatternKind::Where(pattern, condition) => {
                info.with_unresolved_section_index(|info, then_branch| {
                    info.with_unresolved_section_index(|info, condition_branch| {
                        self.gen_sections_from_pattern(
                            pattern,
                            Ok(condition_branch),
                            input_ty,
                            input,
                            result,
                            else_branch,
                            loop_info,
                            sections,
                            info,
                        );

                        sections.add_section()
                    });

                    if let Some(condition) =
                        self.gen_computation_from_expr(condition, loop_info, sections, info)
                    {
                        sections.set_terminator(Terminator::If(
                            condition,
                            then_branch,
                            else_branch,
                        ));
                    }

                    sections.add_section()
                });

                gen_then_branch!(info);
            }
            typecheck::PatternKind::Tuple(patterns) => {
                let tuple_tys = match input_ty {
                    Type::Tuple(tys) => tys,
                    _ => unreachable!(),
                };

                for ((index, pattern), ty) in patterns.iter().enumerate().zip(tuple_tys) {
                    let element = self.new_computation_id();
                    sections.add_statement(Statement::Compute(
                        element,
                        Expression {
                            ty: ty.clone(),
                            kind: ExpressionKind::TupleElement(input, index),
                        },
                    ));

                    info.with_unresolved_section_index(|info, then_branch| {
                        self.gen_sections_from_pattern(
                            pattern,
                            Ok(then_branch),
                            input_ty,
                            element,
                            result,
                            else_branch,
                            loop_info,
                            sections,
                            info,
                        );

                        sections.add_section()
                    });
                }

                gen_then_branch!(info);
            }
            typecheck::PatternKind::Destructure(fields) => {
                let (id, field_tys) = match input_ty {
                    Type::Structure(id, field_tys) => (id, field_tys),
                    _ => unreachable!(),
                };

                for (index, pattern) in fields {
                    let element = self.new_computation_id();
                    sections.add_statement(Statement::Compute(
                        element,
                        Expression {
                            ty: field_tys[*index].clone(),
                            kind: ExpressionKind::StructureElement(input, *id, *index),
                        },
                    ));

                    info.with_unresolved_section_index(|info, then_branch| {
                        self.gen_sections_from_pattern(
                            pattern,
                            Ok(then_branch),
                            input_ty,
                            element,
                            result,
                            else_branch,
                            loop_info,
                            sections,
                            info,
                        );

                        sections.add_section()
                    });
                }

                gen_then_branch!(info);
            }
            typecheck::PatternKind::Variant(input_discriminant, patterns) => {
                let (id, variant_tys) = match input_ty {
                    Type::Enumeration(id, variants_tys) => {
                        (*id, variants_tys[*input_discriminant].clone())
                    }
                    _ => unreachable!(),
                };

                let discriminant = self.new_computation_id();
                sections.add_statement(Statement::Compute(
                    discriminant,
                    Expression {
                        ty: Type::Discriminant,
                        kind: ExpressionKind::Discriminant(input),
                    },
                ));

                let comparison = self.new_computation_id();
                sections.add_statement(Statement::Compute(
                    comparison,
                    Expression {
                        ty: Type::Boolean,
                        kind: ExpressionKind::CompareDiscriminants(
                            discriminant,
                            *input_discriminant,
                        ),
                    },
                ));

                info.with_unresolved_section_index(|_, then_branch| {
                    sections.set_terminator(Terminator::If(comparison, then_branch, else_branch));
                    sections.add_section()
                });

                for (index, pattern) in patterns.iter().enumerate() {
                    let element = self.new_computation_id();
                    sections.add_statement(Statement::Compute(
                        element,
                        Expression {
                            ty: variant_tys[index].clone(),
                            kind: ExpressionKind::VariantElement(
                                input,
                                (id, *input_discriminant),
                                index,
                            ),
                        },
                    ));

                    info.with_unresolved_section_index(|info, then_branch| {
                        self.gen_sections_from_pattern(
                            pattern,
                            Ok(then_branch),
                            input_ty,
                            element,
                            result,
                            else_branch,
                            loop_info,
                            sections,
                            info,
                        );

                        sections.add_section()
                    });
                }

                gen_then_branch!(info);
            }
        }

        assert!(sections.has_terminator());
    }

    fn gen_type(&mut self, ty: typecheck::Type, info: &mut Info) -> Type {
        match ty {
            typecheck::Type::Parameter(_) => unreachable!(),
            typecheck::Type::Named(id, _, structure) => {
                if let Some(ty) = info.nominal_types.get(&id) {
                    return ty.clone();
                }

                let ty = match structure {
                    typecheck::TypeStructure::Marker => Type::Marker,
                    typecheck::TypeStructure::Structure(fields) => Type::Structure(
                        id,
                        fields
                            .into_iter()
                            .map(|ty| self.gen_type(ty, info))
                            .collect(),
                    ),
                    typecheck::TypeStructure::Enumeration(variants) => Type::Enumeration(
                        id,
                        variants
                            .into_iter()
                            .map(|tys| tys.into_iter().map(|ty| self.gen_type(ty, info)).collect())
                            .collect(),
                    ),
                };

                info.nominal_types.insert(id, ty.clone());

                ty
            }
            typecheck::Type::Function(input, output) => Type::Function(
                Box::new(self.gen_type(*input, info)),
                Box::new(self.gen_type(*output, info)),
            ),
            typecheck::Type::Tuple(tys) => {
                Type::Tuple(tys.into_iter().map(|ty| self.gen_type(ty, info)).collect())
            }
            typecheck::Type::Builtin(ty) => match ty {
                typecheck::BuiltinType::Number => Type::Number,
                typecheck::BuiltinType::Text => Type::Text,
                typecheck::BuiltinType::Integer => Type::Integer,
                typecheck::BuiltinType::List(ty) => Type::List(Box::new(self.gen_type(*ty, info))),
                typecheck::BuiltinType::Mutable(ty) => {
                    Type::Mutable(Box::new(self.gen_type(*ty, info)))
                }
            },
            typecheck::Type::Bottom(_) => Type::Unreachable,
        }
    }
}

impl Constant<UnresolvedSectionIndex> {
    fn finalize(self, map: &SectionIndexMap) -> Constant<SectionIndex> {
        Constant {
            ty: self.ty,
            sections: self.sections.finalize(map),
        }
    }
}

impl Function<UnresolvedSectionIndex> {
    fn finalize(self, map: &SectionIndexMap) -> Function {
        Function {
            captures: self.captures,
            locals: self.locals,
            sections: self.sections.finalize(map),
        }
    }
}

impl Sections<UnresolvedSectionIndex> {
    fn finalize(self, map: &SectionIndexMap) -> Sections {
        self.into_iter()
            .map(|section| section.finalize(map))
            .collect()
    }
}

impl Section<UnresolvedSectionIndex> {
    fn finalize(self, map: &SectionIndexMap) -> Section {
        Section {
            statements: self
                .statements
                .into_iter()
                .map(|statement| statement.finalize(map))
                .collect(),
            terminator: self.terminator.map(|terminator| terminator.finalize(map)),
        }
    }
}

impl Terminator<UnresolvedSectionIndex> {
    fn finalize(self, map: &SectionIndexMap) -> Terminator {
        match self {
            Terminator::If(id, then_branch, else_branch) => {
                Terminator::If(id, then_branch.finalize(map), else_branch.finalize(map))
            }
            Terminator::Return(id) => Terminator::Return(id),
            Terminator::Goto(section) => Terminator::Goto(section.finalize(map)),
            Terminator::Unreachable => Terminator::Unreachable,
        }
    }
}

impl Statement<UnresolvedSectionIndex> {
    fn finalize(self, map: &SectionIndexMap) -> Statement {
        match self {
            Statement::Compute(id, expr) => Statement::Compute(id, expr.finalize(map)),
            Statement::Initialize(var, computation) => Statement::Initialize(var, computation),
            Statement::Drop(vars) => Statement::Drop(vars),
        }
    }
}

impl Expression<UnresolvedSectionIndex> {
    fn finalize(self, map: &SectionIndexMap) -> Expression {
        Expression {
            ty: self.ty,
            kind: match self.kind {
                ExpressionKind::Marker => ExpressionKind::Marker,
                ExpressionKind::Constant(id) => ExpressionKind::Constant(id),
                ExpressionKind::Function(function) => {
                    ExpressionKind::Function(function.finalize(map))
                }
                ExpressionKind::Variable(id) => ExpressionKind::Variable(id),
                ExpressionKind::FunctionInput => ExpressionKind::FunctionInput,
                ExpressionKind::Number(number) => ExpressionKind::Number(number),
                ExpressionKind::Text(text) => ExpressionKind::Text(text),
                ExpressionKind::Call(function, input) => ExpressionKind::Call(function, input),
                ExpressionKind::External(lib, identifier, inputs) => {
                    ExpressionKind::External(lib, identifier, inputs)
                }
                ExpressionKind::Tuple(values) => ExpressionKind::Tuple(values),
                ExpressionKind::Structure(values) => ExpressionKind::Structure(values),
                ExpressionKind::Variant(discriminant, values) => {
                    ExpressionKind::Variant(discriminant, values)
                }
                ExpressionKind::TupleElement(tuple, index) => {
                    ExpressionKind::TupleElement(tuple, index)
                }
                ExpressionKind::StructureElement(structure, id, index) => {
                    ExpressionKind::StructureElement(structure, id, index)
                }
                ExpressionKind::VariantElement(variant, (id, discriminant), index) => {
                    ExpressionKind::VariantElement(variant, (id, discriminant), index)
                }
                ExpressionKind::Discriminant(variant) => ExpressionKind::Discriminant(variant),
                ExpressionKind::CompareDiscriminants(left, right) => {
                    ExpressionKind::CompareDiscriminants(left, right)
                }
            },
        }
    }
}

impl UnresolvedSectionIndex {
    fn finalize(self, map: &SectionIndexMap) -> SectionIndex {
        match self {
            UnresolvedSectionIndex::Resolved(index) => index,
            UnresolvedSectionIndex::Unresolved(index) => map.get(&index).unwrap().unwrap(),
        }
    }
}
