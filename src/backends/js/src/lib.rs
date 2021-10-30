use resast::prelude::*;
use std::borrow::Cow;
use wipple_frontend::{
    id::VariableId,
    typecheck::{Item, ItemKind},
};

pub fn gen(item: &Item) -> String {
    let expr = gen_item(item, &mut Info::default());

    let program = Program::Script(vec![
        ProgramPart::Dir(Dir {
            expr: Lit::String(StringLit::Double(Cow::Borrowed("use strict"))),
            dir: Cow::Borrowed("use strict"),
        }),
        ProgramPart::Stmt(Stmt::Expr(expr)),
    ]);

    let mut s = resw::write_str::WriteString::new();
    let mut writer = resw::Writer::new(&mut s);
    writer.write_program(&program).unwrap();
    s.get_string().unwrap()
}

#[derive(Default)]
struct Info {
    variables: Vec<VariableId>,
}

fn gen_item<'a>(item: &'a Item, info: &mut Info) -> Expr<'a> {
    match &item.kind {
        ItemKind::Unit => Expr::Lit(Lit::Null),
        ItemKind::Number { value } => {
            // TODO: Decimal support
            Expr::Lit(Lit::Number(Cow::Owned(value.to_string())))
        }
        ItemKind::Text { value } => Expr::Lit(Lit::String(StringLit::Double(Cow::Owned(
            value.to_string(),
        )))),
        ItemKind::Block { statements } => {
            let mut info = Info::default();
            let statement_count = statements.len();

            let statements = statements
                .iter()
                .enumerate()
                .map(|(index, statement)| {
                    let expr = gen_item(statement, &mut info);

                    ProgramPart::Stmt(if index + 1 == statement_count {
                        Stmt::Return(Some(expr))
                    } else {
                        Stmt::Expr(expr)
                    })
                })
                .collect::<Vec<_>>();

            let decls = info
                .variables
                .into_iter()
                .map(|variable| VarDecl {
                    id: Pat::Ident(mangle(variable)),
                    init: None,
                })
                .collect::<Vec<_>>();

            let statements = (!decls.is_empty())
                .then(|| ProgramPart::Decl(Decl::Var(VarKind::Let, decls)))
                .into_iter()
                .chain(statements)
                .collect();

            Expr::Call(CallExpr {
                callee: Box::new(Expr::ArrowFunc(ArrowFuncExpr {
                    id: None,
                    params: Vec::new(),
                    body: ArrowFuncBody::FuncBody(FuncBody(statements)),
                    expression: true,
                    generator: false,
                    is_async: false,
                })),
                arguments: Vec::new(),
            })
        }
        ItemKind::Apply { function, input } => Expr::Call(CallExpr {
            callee: Box::new(gen_item(function, info)),
            arguments: vec![gen_item(input, info)],
        }),
        ItemKind::Initialize { variable, value } => {
            info.variables.push(*variable);

            Expr::Assign(AssignExpr {
                operator: AssignOp::Equal,
                left: AssignLeft::Pat(Pat::Ident(mangle(*variable))),
                right: Box::new(gen_item(value, info)),
            })
        }
        ItemKind::Variable { variable } => Expr::Ident(mangle(*variable)),
        ItemKind::Function { body, .. } => Expr::ArrowFunc(ArrowFuncExpr {
            id: None,
            params: vec![FuncArg::Pat(Pat::Ident(mangle_function_input()))],
            body: ArrowFuncBody::Expr(Box::new(gen_item(body, info))),
            expression: true,
            generator: false,
            is_async: false,
        }),
        ItemKind::FunctionInput => Expr::Ident(mangle_function_input()),
        ItemKind::External { .. } => todo!(),
    }
}

fn mangle<'a>(variable: VariableId) -> Ident<'a> {
    Ident::new(format!("wpl${}", variable.0))
}

fn mangle_function_input<'a>() -> Ident<'a> {
    Ident::from("wpl$input")
}
