use crate::lowering::*;
use serde::Serialize;
use std::{collections::HashSet, sync::Arc};
use wipple_diagnostics::*;

id! {
    pub struct ExprId;
}

#[derive(Debug, Clone, Serialize)]
pub struct Expr {
    pub id: ExprId,
    pub span: Span,
    pub kind: ExprKind,
    pub references: Arc<HashSet<Variable>>,
}

#[derive(Debug, Clone, Serialize)]
pub enum ExprKind {
    Unit,
    Constant(LoweredConstantExprKind),
    Declare(Variable),
    Initialize(Variable, Box<Expr>),
    Block(Vec<Expr>),
    RuntimeVariable(Variable),
}

impl Expr {
    fn new(span: Span, kind: ExprKind, references: Arc<HashSet<Variable>>) -> Self {
        Expr {
            id: ExprId::new(),
            span,
            kind,
            references,
        }
    }
}

pub fn analyze(expr: LoweredExpr, diagnostics: &mut Diagnostics) -> Option<Expr> {
    struct Context<'a> {
        mask: &'a mut HashSet<Variable>,
        used: &'a mut HashSet<VariableId>,
        top_level: bool,
        diagnostics: &'a mut Diagnostics,
    }

    fn analyze(expr: LoweredExpr, ctx: &mut Context) -> Option<Expr> {
        let (kind, references) = match expr.kind {
            LoweredExprKind::Unit(_) => (ExprKind::Unit, Default::default()),
            LoweredExprKind::Constant(constant_expr) => {
                (ExprKind::Constant(constant_expr.kind), Default::default())
            }
            LoweredExprKind::Declare(declare_expr) => {
                (ExprKind::Declare(declare_expr.variable), Default::default())
            }
            LoweredExprKind::Initialize(initialize_expr) => {
                let expr = analyze(*initialize_expr.value, ctx)?;
                let references = expr.references.clone();

                (
                    ExprKind::Initialize(initialize_expr.variable, Box::new(expr)),
                    references,
                )
            }
            LoweredExprKind::Block(block_expr) => {
                let mut mask = HashSet::new();
                let mut used = HashSet::new();

                let mut inner_ctx = Context {
                    mask: &mut mask,
                    used: &mut used,
                    top_level: false,
                    diagnostics: ctx.diagnostics,
                };

                let mut statements = Vec::with_capacity(block_expr.statements.len());
                let mut references = HashSet::new();
                let mut error = false;
                for (index, statement) in block_expr.statements.into_iter().enumerate() {
                    if let Some(expr) = analyze(statement, &mut inner_ctx) {
                        if let ExprKind::Declare(variable) | ExprKind::Initialize(variable, _) =
                            expr.kind
                        {
                            inner_ctx.mask.insert(variable);
                        }

                        if index < statements.capacity() - 1
                            && matches!(
                                expr.kind,
                                ExprKind::Unit
                                    | ExprKind::Constant(_)
                                    | ExprKind::RuntimeVariable(_)
                            )
                        {
                            inner_ctx.diagnostics.add(Diagnostic::new(
                                DiagnosticLevel::Warning,
                                "Unused expression",
                                vec![Note::primary(expr.span, "This expression has no effect")],
                            ));
                        }

                        references.extend(expr.references.iter());
                        statements.push(expr);
                    } else {
                        error = true
                    }
                }

                if !ctx.top_level {
                    for variable in inner_ctx.mask.iter() {
                        if !inner_ctx.used.contains(&variable.id) {
                            inner_ctx.diagnostics.add(Diagnostic::new(
                                DiagnosticLevel::Warning,
                                format!("'{}' is unused", variable.name),
                                vec![Note::primary(variable.declaration_span, "Unused variable")],
                            ));
                        }
                    }
                }

                if error {
                    return None;
                }

                (ExprKind::Block(statements), Arc::new(references))
            }
            LoweredExprKind::DataBlock(_) => todo!(),
            LoweredExprKind::Apply(_) => todo!(),
            LoweredExprKind::RuntimeVariable(runtime_variable_expr) => {
                ctx.used.insert(runtime_variable_expr.variable.id);

                let mut references = HashSet::new();
                references.insert(runtime_variable_expr.variable);

                (
                    ExprKind::RuntimeVariable(runtime_variable_expr.variable),
                    Arc::new(references),
                )
            }
            LoweredExprKind::Operator(_) => todo!(),
            LoweredExprKind::ApplyOperator(_) => todo!(),
            LoweredExprKind::PartiallyApplyLeftOfOperator(_) => todo!(),
            LoweredExprKind::PartiallyApplyRightOfOperator(_) => todo!(),
            LoweredExprKind::ExternalReference(_) => todo!(),
            LoweredExprKind::Builtin(_) => todo!(),
            LoweredExprKind::Error => return None,
        };

        Some(Expr::new(expr.span, kind, references))
    }

    let mut ctx = Context {
        mask: &mut HashSet::new(),
        used: &mut HashSet::new(),
        top_level: true,
        diagnostics,
    };

    analyze(expr, &mut ctx)
}
