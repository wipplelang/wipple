#![allow(clippy::too_many_arguments, clippy::new_without_default)]

mod format;

use crate::{
    analysis::typecheck, helpers::InternedString, Compiler, IrComputationId, Loader,
    MonomorphizedConstantId, VariableId,
};
use std::{
    collections::{BTreeMap, BTreeSet},
    mem,
};

pub mod abi {
    pub const RUNTIME: &str = "runtime";
}

#[derive(Debug, Clone)]
pub struct Program {
    pub constants: Vec<Constant>,
    pub functions: Vec<Function>,
    pub entrypoint: Sections,
}

#[derive(Debug, Clone)]
pub struct Section<I = SectionIndex> {
    pub statements: Vec<Statement>,
    pub terminator: Option<Terminator<I>>,
}

impl<I> Section<I> {
    pub fn new() -> Self {
        Section {
            statements: Vec::new(),
            terminator: None,
        }
    }

    fn add_statement(&mut self, statement: Statement) {
        assert!(self.terminator.is_none());
        self.statements.push(statement);
    }

    fn set_terminator(&mut self, terminator: Terminator<I>) {
        self.terminator = Some(terminator);
    }

    fn has_terminator(&self) -> bool {
        self.terminator.is_some()
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Marker,
    Constant(usize),
    Function(usize),
    Variable(VariableId),
    FunctionInput,
    Number(f64),
    Text(InternedString),
    Call(IrComputationId, IrComputationId),
    External(InternedString, InternedString, Vec<IrComputationId>),
    Tuple(Vec<IrComputationId>),
    Variant(usize, Vec<IrComputationId>),
    TupleElement(IrComputationId, usize),
    Discriminant(IrComputationId),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Compute(IrComputationId, Expression),
    Initialize(VariableId, IrComputationId),
}

#[derive(Debug, Clone)]
pub enum Terminator<I = SectionIndex> {
    If(IrComputationId, I, I),
    Return(IrComputationId),
    Goto(I),
    Unreachable,
}

#[derive(Debug, Clone)]
pub struct Constant<I = SectionIndex> {
    pub ty: typecheck::Type,
    pub sections: Sections<I>,
}

#[derive(Debug, Clone)]
pub struct Function<I = SectionIndex> {
    /// The input and output types of the function, or `None` for the entrypoint.
    pub ty: Option<(typecheck::Type, typecheck::Type)>,

    /// The set of variables to capture in the stack where the function is
    /// declared. At runtime, a structure will be created and these variables'
    /// values will be copied into the structure.
    pub captures: BTreeSet<VariableId>,

    pub sections: Sections<I>,
}

#[derive(Debug, Clone)]
pub struct Sections<I = SectionIndex>(Vec<Section<I>>);

impl<I> Sections<I> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Sections(vec![Section::new()])
    }

    pub fn enumerate(&self) -> impl Iterator<Item = (SectionIndex, &Section<I>)> {
        self.0
            .iter()
            .enumerate()
            .map(|(index, section)| (SectionIndex(index), section))
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

    fn add_statement(&mut self, statement: Statement) {
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
struct UnresolvedSectionIndex(usize);

impl<L: Loader> Compiler<L> {
    pub fn ir_from(&mut self, program: typecheck::Program) -> Program {
        let mut constants = BTreeMap::new();
        let mut functions = Vec::new();

        let mut info = Info {
            constants: &mut constants,
            functions: &mut functions,
            map: BTreeMap::new(),
            captures: BTreeSet::new(),
            locals: BTreeSet::new(),
        };

        let mut constants = Vec::new();
        for (id, ((), constant)) in program.declarations.monomorphized_constants {
            let ty = constant.value.ty.clone();
            info.constants.insert(id, constants.len());

            let mut sections = Sections::new();
            let result = self.gen_computation_from_expr(constant.value, &mut sections, &mut info);
            sections.set_terminator(Terminator::Return(result));

            constants.push(Constant { ty, sections });
        }

        let mut entrypoint = Sections::new();
        for expr in program.body {
            self.gen_computation_from_expr(expr, &mut entrypoint, &mut info);
        }

        let map = info.map;

        Program {
            constants: constants
                .into_iter()
                .map(|constant| constant.finalize(&map))
                .collect(),
            functions: functions
                .into_iter()
                .map(|function| function.finalize(&map))
                .collect(),
            entrypoint: entrypoint.finalize(&map),
        }
    }
}

type SectionIndexMap = BTreeMap<UnresolvedSectionIndex, Option<SectionIndex>>;

struct Info<'a> {
    constants: &'a mut BTreeMap<MonomorphizedConstantId, usize>,
    functions: &'a mut Vec<Function<UnresolvedSectionIndex>>,
    map: SectionIndexMap,
    captures: BTreeSet<VariableId>,
    locals: BTreeSet<VariableId>,
}

impl<'a> Info<'a> {
    fn with_unresolved_section_index(
        &mut self,
        f: impl FnOnce(&mut Self, UnresolvedSectionIndex) -> SectionIndex,
    ) -> SectionIndex {
        let unresolved = UnresolvedSectionIndex(self.map.len());
        self.map.insert(unresolved, None);
        let index = f(self, unresolved);
        self.map.insert(unresolved, Some(index));
        index
    }
}

impl<L: Loader> Compiler<L> {
    fn gen_computation_from_expr(
        &mut self,
        expr: typecheck::Expression,
        sections: &mut Sections<UnresolvedSectionIndex>,
        info: &mut Info,
    ) -> IrComputationId {
        let id = self.new_ir_computation_id();
        self.gen_computation_from_expr_with(id, expr, sections, info);
        id
    }

    fn gen_computation_from_expr_with(
        &mut self,
        computation: IrComputationId,
        expr: typecheck::Expression,
        sections: &mut Sections<UnresolvedSectionIndex>,
        info: &mut Info,
    ) {
        match expr.kind {
            typecheck::ExpressionKind::Marker => {
                sections.add_statement(Statement::Compute(computation, Expression::Marker));
            }
            typecheck::ExpressionKind::Variable(var) => {
                if !info.locals.contains(&var) {
                    info.captures.insert(var);
                }

                sections.add_statement(Statement::Compute(computation, Expression::Variable(var)));
            }
            typecheck::ExpressionKind::Text(text) => {
                sections.add_statement(Statement::Compute(computation, Expression::Text(text)));
            }
            typecheck::ExpressionKind::Number(number) => {
                sections.add_statement(Statement::Compute(computation, Expression::Number(number)));
            }
            typecheck::ExpressionKind::Block(exprs) => {
                let count = exprs.len();

                for (index, expr) in exprs.into_iter().enumerate() {
                    if index + 1 == count {
                        self.gen_computation_from_expr_with(computation, expr, sections, info);
                    } else {
                        self.gen_computation_from_expr(expr, sections, info);
                    }
                }
            }
            typecheck::ExpressionKind::Call(function, input) => {
                let output_ty = match &function.ty {
                    typecheck::Type::Function(_, output) => output.as_ref(),
                    _ => unreachable!(),
                };

                let no_return = matches!(output_ty, typecheck::Type::Bottom(_));

                let function = self.gen_computation_from_expr(*function, sections, info);
                let input = self.gen_computation_from_expr(*input, sections, info);

                sections.add_statement(Statement::Compute(
                    computation,
                    Expression::Call(function, input),
                ));

                if no_return {
                    sections.set_terminator(Terminator::Unreachable);
                    sections.add_section(); // this (unused) section will be eliminated during optimization
                }
            }
            typecheck::ExpressionKind::Function(pattern, body) => {
                let prev_sections = mem::replace(sections, Sections::new());
                let prev_captures = mem::take(&mut info.captures);

                let input = self.new_ir_computation_id();
                sections.add_statement(Statement::Compute(input, Expression::FunctionInput));

                self.gen_sections_from_init(pattern, input, computation, sections, info);

                let result = self.gen_computation_from_expr(*body, sections, info);
                sections.set_terminator(Terminator::Return(result));

                let (input_ty, output_ty) = match expr.ty {
                    typecheck::Type::Function(input, output) => (*input, *output),
                    _ => unreachable!(),
                };

                let mut captures = prev_captures.clone();
                captures.extend(mem::replace(&mut info.captures, prev_captures));

                let function = Function {
                    ty: Some((input_ty, output_ty)),
                    captures: captures.clone(),
                    sections: mem::replace(sections, prev_sections),
                };

                let id = info.functions.len();
                info.functions.push(function);

                sections.add_statement(Statement::Compute(computation, Expression::Function(id)));
            }
            typecheck::ExpressionKind::When(input, arms) => {
                let input = self.gen_computation_from_expr(*input, sections, info);
                self.gen_sections_from_when(arms, input, computation, sections, info);
            }
            typecheck::ExpressionKind::External(abi, identifier, inputs) => {
                let inputs = inputs
                    .into_iter()
                    .map(|expr| self.gen_computation_from_expr(expr, sections, info))
                    .collect();

                sections.add_statement(Statement::Compute(
                    computation,
                    Expression::External(abi, identifier, inputs),
                ));
            }
            typecheck::ExpressionKind::Initialize(pattern, value) => {
                let value = self.gen_computation_from_expr(*value, sections, info);
                self.gen_sections_from_init(pattern, value, computation, sections, info);
                // No need to add a marker computation here because variable assignment cannot be
                // used in expression position, so its computation is never used
            }
            typecheck::ExpressionKind::Structure(exprs)
            | typecheck::ExpressionKind::Tuple(exprs) => {
                let values = exprs
                    .into_iter()
                    .map(|expr| self.gen_computation_from_expr(expr, sections, info))
                    .collect();

                sections.add_statement(Statement::Compute(computation, Expression::Tuple(values)));
            }
            typecheck::ExpressionKind::Variant(discriminant, exprs) => {
                let values = exprs
                    .into_iter()
                    .map(|expr| self.gen_computation_from_expr(expr, sections, info))
                    .collect();

                sections.add_statement(Statement::Compute(
                    computation,
                    Expression::Variant(discriminant, values),
                ));
            }
            typecheck::ExpressionKind::Return(expr) => {
                let result = self.gen_computation_from_expr(*expr, sections, info);
                sections.set_terminator(Terminator::Return(result));
                sections.add_section();
            }
            typecheck::ExpressionKind::Loop(_) => todo!(),
            typecheck::ExpressionKind::Break(_) => todo!(),
            typecheck::ExpressionKind::Continue => todo!(),
            typecheck::ExpressionKind::Constant(id) => {
                let constant = info.constants.get(&id).unwrap();

                sections.add_statement(Statement::Compute(
                    computation,
                    Expression::Constant(*constant),
                ));
            }
        }

        assert!(!sections.has_terminator());
    }

    fn gen_sections_from_init(
        &mut self,
        pattern: typecheck::Pattern,
        input: IrComputationId,
        result: IrComputationId,
        sections: &mut Sections<UnresolvedSectionIndex>,
        info: &mut Info,
    ) {
        let span = pattern.span;

        self.gen_sections_from_when(
            vec![typecheck::Arm {
                span,
                pattern,
                body: typecheck::Expression {
                    span,
                    ty: typecheck::Type::Tuple(Vec::new()),
                    kind: typecheck::ExpressionKind::Marker,
                },
            }],
            input,
            result,
            sections,
            info,
        );
    }

    fn gen_sections_from_when(
        &mut self,
        arms: Vec<typecheck::Arm>,
        input: IrComputationId,
        result: IrComputationId,
        sections: &mut Sections<UnresolvedSectionIndex>,
        info: &mut Info,
    ) {
        info.with_unresolved_section_index(|info, end| {
            for arm in arms {
                info.with_unresolved_section_index(|info, else_branch| {
                    self.gen_sections_from_pattern(
                        arm.pattern,
                        Err((arm.body, end)),
                        input,
                        result,
                        else_branch,
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
        pattern: typecheck::Pattern,
        body: Result<UnresolvedSectionIndex, (typecheck::Expression, UnresolvedSectionIndex)>,
        input: IrComputationId,
        result: IrComputationId,
        else_branch: UnresolvedSectionIndex,
        sections: &mut Sections<UnresolvedSectionIndex>,
        info: &mut Info,
    ) {
        macro_rules! gen_then_branch {
            ($info:ident) => {
                match body {
                    Ok(section) => sections.set_terminator(Terminator::Goto(section)),
                    Err((expr, end)) => {
                        self.gen_computation_from_expr_with(result, expr, sections, $info);
                        sections.set_terminator(Terminator::Goto(end));
                    }
                }
            };
        }

        match pattern.kind {
            typecheck::PatternKind::Wildcard => gen_then_branch!(info),
            typecheck::PatternKind::Number(number) => {
                let predicate = self.new_ir_computation_id();
                sections.add_statement(Statement::Compute(predicate, Expression::Number(number)));

                let condition = self.new_ir_computation_id();
                sections.add_statement(Statement::Compute(
                    condition,
                    Expression::External(
                        InternedString::new(abi::RUNTIME),
                        InternedString::new("number-equality"),
                        vec![input, predicate],
                    ),
                ));

                info.with_unresolved_section_index(|_, then_branch| {
                    sections.set_terminator(Terminator::If(condition, then_branch, else_branch));
                    sections.add_section()
                });

                gen_then_branch!(info);
            }
            typecheck::PatternKind::Text(text) => {
                let predicate = self.new_ir_computation_id();
                sections.add_statement(Statement::Compute(predicate, Expression::Text(text)));

                let condition = self.new_ir_computation_id();
                sections.add_statement(Statement::Compute(
                    condition,
                    Expression::External(
                        InternedString::new(abi::RUNTIME),
                        InternedString::new("text-equality"),
                        vec![input, predicate],
                    ),
                ));

                info.with_unresolved_section_index(|_, then_branch| {
                    sections.set_terminator(Terminator::If(condition, then_branch, else_branch));
                    sections.add_section()
                });

                gen_then_branch!(info);
            }
            typecheck::PatternKind::Variable(var) => {
                info.locals.insert(var);
                sections.add_statement(Statement::Initialize(var, input));
                gen_then_branch!(info);
            }
            typecheck::PatternKind::Or(left, right) => {
                info.with_unresolved_section_index(|info, then_branch| {
                    info.with_unresolved_section_index(|info, else_if_branch| {
                        self.gen_sections_from_pattern(
                            *left,
                            Ok(then_branch),
                            input,
                            result,
                            else_if_branch,
                            sections,
                            info,
                        );

                        sections.add_section()
                    });

                    self.gen_sections_from_pattern(
                        *right,
                        Ok(then_branch),
                        input,
                        result,
                        else_branch,
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
                            *pattern,
                            Ok(condition_branch),
                            input,
                            result,
                            else_branch,
                            sections,
                            info,
                        );

                        sections.add_section()
                    });

                    let condition = self.gen_computation_from_expr(*condition, sections, info);
                    sections.set_terminator(Terminator::If(condition, then_branch, else_branch));

                    sections.add_section()
                });

                gen_then_branch!(info);
            }
            typecheck::PatternKind::Tuple(patterns) => {
                for (index, pattern) in patterns.into_iter().enumerate() {
                    let element = self.new_ir_computation_id();
                    sections.add_statement(Statement::Compute(
                        element,
                        Expression::TupleElement(input, index),
                    ));

                    info.with_unresolved_section_index(|info, then_branch| {
                        self.gen_sections_from_pattern(
                            pattern,
                            Ok(then_branch),
                            element,
                            result,
                            else_branch,
                            sections,
                            info,
                        );

                        sections.add_section()
                    });
                }

                gen_then_branch!(info);
            }
            typecheck::PatternKind::Destructure(_) => todo!(),
            typecheck::PatternKind::Variant(_, _) => todo!(),
        }

        assert!(sections.has_terminator());
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
            ty: self.ty,
            captures: self.captures,
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
            statements: self.statements,
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

impl UnresolvedSectionIndex {
    fn finalize(self, map: &SectionIndexMap) -> SectionIndex {
        map.get(&self).unwrap().unwrap()
    }
}
