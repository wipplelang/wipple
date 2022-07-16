mod pretty;

use crate::{
    analysis::typecheck, helpers::InternedString, parse::Span, Compiler, IrComputationId, Loader,
    MonomorphizedConstantId, VariableId,
};
use std::{collections::BTreeSet, mem};

pub mod abi {
    pub const RUNTIME: &str = "runtime";
}

#[derive(Debug, Clone)]
pub struct Program {
    // pub constants: BTreeMap<MonomorphizedConstantId, Constant>,
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone, Default)]
pub struct Section {
    pub statements: Vec<Statement>,
    terminator: Option<Terminator>,
}

impl Section {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn terminator(&self) -> &Terminator {
        self.terminator.as_ref().unwrap()
    }

    pub fn into_terminator(self) -> Terminator {
        self.terminator.unwrap()
    }

    fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }

    fn set_terminator(&mut self, terminator: Terminator) {
        self.terminator = Some(terminator);
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Reference {
    Constant(MonomorphizedConstantId),
    Variable(VariableId),
    FunctionInput,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Marker,
    Reference(Reference),
    Function(usize),
    Number(f64),
    Text(InternedString),
    Call(IrComputationId, IrComputationId),
    External(InternedString, InternedString, Vec<IrComputationId>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Compute(IrComputationId, Expression),
    Initialize(VariableId, IrComputationId),
}

#[derive(Debug, Clone)]
pub enum Terminator {
    If(IrComputationId, SectionIndex, SectionIndex),
    Return(IrComputationId),
    Goto(SectionIndex),
    Unreachable,
}

#[derive(Debug, Clone)]
pub struct Function {
    /// The input and output types of the function, or `None` for the entrypoint.
    pub ty: Option<(typecheck::Type, typecheck::Type)>,

    /// The set of variables to capture in the stack where the function is
    /// declared. At runtime, a structure will be created and these variables'
    /// values will be copied into the structure.
    pub captures: BTreeSet<VariableId>,

    pub sections: Sections,
}

#[derive(Debug, Clone)]
pub struct Sections(Vec<Section>);

impl Sections {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Sections(vec![Section::default()])
    }

    pub fn add_section(&mut self) {
        self.add_section_with(Section::default());
    }

    pub fn add_section_with(&mut self, section: Section) {
        self.0.push(section);
    }

    pub fn add_statement(&mut self, statement: Statement) {
        self.0.last_mut().unwrap().add_statement(statement);
    }

    pub fn set_terminator(&mut self, terminator: Terminator) {
        self.0.last_mut().unwrap().set_terminator(terminator);
    }

    pub fn enumerate(&self) -> impl Iterator<Item = (SectionIndex, &Section)> {
        self.0
            .iter()
            .enumerate()
            .map(|(index, section)| (SectionIndex(index), section))
    }

    fn next_index(&self) -> SectionIndex {
        SectionIndex(self.0.len())
    }
}

impl std::ops::Index<SectionIndex> for Sections {
    type Output = Section;

    fn index(&self, index: SectionIndex) -> &Self::Output {
        &self.0[index.0]
    }
}

impl std::ops::IndexMut<SectionIndex> for Sections {
    fn index_mut(&mut self, index: SectionIndex) -> &mut Self::Output {
        &mut self.0[index.0]
    }
}

impl IntoIterator for Sections {
    type Item = Section;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Extend<Section> for Sections {
    fn extend<T: IntoIterator<Item = Section>>(&mut self, iter: T) {
        self.0.extend(iter)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SectionIndex(pub usize);

impl<L: Loader> Compiler<L> {
    pub fn ir_from(&mut self, program: typecheck::Program) -> Program {
        // let mut constants = BTreeMap::new();
        let mut functions = Vec::new();

        let mut info = Info {
            // _constants: &mut constants,
            functions: &mut functions,
            captures: BTreeSet::new(),
            locals: BTreeSet::new(),
        };

        let mut sections = Sections::new();
        for expr in program.body {
            self.gen_computation_from_expr(expr, &mut sections, &mut info);
        }

        let result = self.gen_computation_from_expr(
            typecheck::Expression {
                span: Span::builtin("dummy"),
                ty: typecheck::Type::Tuple(Vec::new()),
                kind: typecheck::ExpressionKind::Marker,
            },
            &mut sections,
            &mut info,
        );

        sections.set_terminator(Terminator::Return(result));

        let function = Function {
            ty: None,
            captures: BTreeSet::new(),
            sections,
        };

        info.functions.push(function);

        Program {
            // constants,
            functions,
        }
    }
}

struct Info<'a> {
    // _constants: &'a mut BTreeMap<MonomorphizedConstantId, Constant>,
    functions: &'a mut Vec<Function>,
    captures: BTreeSet<VariableId>,
    locals: BTreeSet<VariableId>,
}

impl<L: Loader> Compiler<L> {
    fn gen_computation_from_expr(
        &mut self,
        expr: typecheck::Expression,
        sections: &mut Sections,
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
        sections: &mut Sections,
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

                sections.add_statement(Statement::Compute(
                    computation,
                    Expression::Reference(Reference::Variable(var)),
                ));
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
                sections.add_statement(Statement::Compute(
                    input,
                    Expression::Reference(Reference::FunctionInput),
                ));

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
            typecheck::ExpressionKind::Structure(_) => todo!(),
            typecheck::ExpressionKind::Variant(_, _) => todo!(),
            typecheck::ExpressionKind::Return(_) => todo!(),
            typecheck::ExpressionKind::Loop(_) => todo!(),
            typecheck::ExpressionKind::Break(_) => todo!(),
            typecheck::ExpressionKind::Continue => todo!(),
            typecheck::ExpressionKind::Tuple(_) => todo!(),
            typecheck::ExpressionKind::Constant(_) => todo!(),
        };
    }

    fn gen_sections_from_init(
        &mut self,
        pattern: typecheck::Pattern,
        input: IrComputationId,
        result: IrComputationId,
        sections: &mut Sections,
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
        sections: &mut Sections,
        info: &mut Info,
    ) {
        let end = sections.next_index();

        let start = SectionIndex(end.0 + 1);
        sections.set_terminator(Terminator::Goto(start));

        sections.add_section();

        for arm in arms.into_iter().map(Some).chain(std::iter::once(None)) {
            sections.add_section();

            if let Some(arm) = arm {
                let else_branch = SectionIndex(sections.next_index().0 + 1);
                self.gen_sections_from_arm(arm, input, else_branch, end, result, sections, info);
            } else {
                sections.set_terminator(Terminator::Unreachable);
            }
        }

        let next_index = sections.next_index();
        sections[end].set_terminator(Terminator::Goto(next_index));
        sections.add_section();
    }

    #[allow(clippy::too_many_arguments)]
    fn gen_sections_from_arm(
        &mut self,
        arm: typecheck::Arm,
        input: IrComputationId,
        else_branch: SectionIndex,
        end: SectionIndex,
        result: IrComputationId,
        sections: &mut Sections,
        info: &mut Info,
    ) {
        match arm.pattern.kind {
            typecheck::PatternKind::Wildcard => {
                self.gen_computation_from_expr_with(result, arm.body, sections, info);
                sections.set_terminator(Terminator::Goto(end));
            }
            typecheck::PatternKind::Number(number) => {
                let predicate = self.new_ir_computation_id();
                sections.add_statement(Statement::Compute(predicate, Expression::Number(number)));

                let condition = self.new_ir_computation_id();
                sections.add_statement(Statement::Compute(
                    condition,
                    Expression::External(
                        InternedString::new(abi::RUNTIME),
                        InternedString::new("number-equality"),
                        vec![predicate, input],
                    ),
                ));

                let then_branch = sections.next_index();
                sections.set_terminator(Terminator::If(condition, then_branch, else_branch));

                // then branch
                sections.add_section();
                self.gen_computation_from_expr_with(result, arm.body, sections, info);
                sections.set_terminator(Terminator::Goto(end));
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
                        vec![predicate, input],
                    ),
                ));

                let then_branch = sections.next_index();
                sections.set_terminator(Terminator::If(condition, then_branch, else_branch));

                // then branch
                sections.add_section();
                self.gen_computation_from_expr_with(result, arm.body, sections, info);
                sections.set_terminator(Terminator::Goto(end));
            }
            typecheck::PatternKind::Variable(var) => {
                info.locals.insert(var);
                sections.add_statement(Statement::Initialize(var, input));
                self.gen_computation_from_expr_with(result, arm.body, sections, info);
                sections.set_terminator(Terminator::Goto(end));
            }
            typecheck::PatternKind::Or(left, right) => {
                let else_if_branch = SectionIndex(sections.next_index().0 + 1);

                // FIXME: Don't duplicate the entire arm body, create a new section instead
                self.gen_sections_from_arm(
                    typecheck::Arm {
                        span: arm.span,
                        pattern: *right,
                        body: arm.body.clone(),
                    },
                    input,
                    else_branch,
                    end,
                    result,
                    sections,
                    info,
                );

                sections.add_section();

                self.gen_sections_from_arm(
                    typecheck::Arm {
                        span: arm.span,
                        pattern: *left,
                        body: arm.body.clone(),
                    },
                    input,
                    else_if_branch,
                    end,
                    result,
                    sections,
                    info,
                );
            }
            typecheck::PatternKind::Where(_, _) => todo!(),
            typecheck::PatternKind::Tuple(_) => todo!(),
            typecheck::PatternKind::Destructure(_) => todo!(),
            typecheck::PatternKind::Variant(_, _) => todo!(),
        }
    }
}
