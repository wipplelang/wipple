use resast::prelude::*;
use std::borrow::Cow;
use wipple_frontend::{id::VariableId, *};

pub fn gen(item: Item) -> String {
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

fn gen_item<'a>(item: Item, info: &mut Info) -> Expr<'a> {
    match item.kind {
        ItemKind::Error => panic!("Must provide fully-typechecked item"),
        ItemKind::Unit(_) => Expr::Lit(Lit::Null),
        ItemKind::Constant(constant_item) => match constant_item.kind {
            ConstantItemKind::Number(number) => {
                // TODO: Decimal support
                Expr::Lit(Lit::Number(Cow::Owned(number.to_string())))
            }
            ConstantItemKind::Text(text) => {
                Expr::Lit(Lit::String(StringLit::Double(Cow::Owned(text.to_string()))))
            }
        },
        ItemKind::Block(block_item) => {
            let mut info = Info::default();
            let statement_count = block_item.statements.len();

            let statements = block_item
                .statements
                .into_iter()
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
        ItemKind::Apply(apply_item) => Expr::Call(CallExpr {
            callee: Box::new(gen_item(*apply_item.function, info)),
            arguments: vec![gen_item(*apply_item.input, info)],
        }),
        ItemKind::Initialize(initialize_item) => {
            info.variables.push(initialize_item.variable);

            Expr::Assign(AssignExpr {
                operator: AssignOp::Equal,
                left: AssignLeft::Pat(Pat::Ident(mangle(initialize_item.variable))),
                right: Box::new(gen_item(*initialize_item.value, info)),
            })
        }
        ItemKind::Variable(variable_item) => Expr::Ident(mangle(variable_item.variable)),
        ItemKind::Function(function_item) => Expr::ArrowFunc(ArrowFuncExpr {
            id: None,
            params: vec![FuncArg::Pat(Pat::Ident(mangle_function_input()))],
            body: ArrowFuncBody::Expr(Box::new(gen_item(*function_item.body, info))),
            expression: true,
            generator: false,
            is_async: false,
        }),
        ItemKind::FunctionInput(_) => Expr::Ident(mangle_function_input()),
        ItemKind::External(_) => todo!(),
    }
}

fn mangle<'a>(variable: VariableId) -> Ident<'a> {
    Ident::new(format!("wpl${}", variable.0))
}

fn mangle_function_input<'a>() -> Ident<'a> {
    Ident::from("wpl$input")
}
