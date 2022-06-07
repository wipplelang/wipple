use resast::prelude::*;
use std::borrow::Cow;

pub fn compile(program: wipple_compiler::optimize::Program) -> String {
    let program = Program::Script(
        program
            .constants
            .into_iter()
            .map(|(id, expr)| ProgramPart::Stmt(compile_constant(id, expr)))
            .chain(
                program
                    .body
                    .into_iter()
                    .flat_map(compile_statement)
                    .map(ProgramPart::Stmt),
            )
            .collect(),
    );

    let builtins = String::from(include_str!("./builtins.js"));

    let mut s = resw::write_str::WriteString::new();
    let mut writer = resw::Writer::new(&mut s);
    writer.write_program(&program).unwrap();

    let body = s.get_string().unwrap();
    builtins + &body
}

fn compile_constant(
    constant: wipple_compiler::MonomorphizedConstantId,
    expr: wipple_compiler::optimize::Expression,
) -> Stmt<'static> {
    Stmt::Var(vec![VarDecl {
        id: Pat::Ident(mangle_constant(constant)),
        init: Some(compile_expression(expr)),
    }])
}

fn compile_pattern(
    pattern: wipple_compiler::optimize::Pattern,
    value: Expr<'static>,
) -> Vec<Stmt<'static>> {
    fn compile_pattern(
        pattern: wipple_compiler::optimize::Pattern,
        value: Expr<'static>,
        result: &mut Vec<Stmt<'static>>,
    ) {
        use wipple_compiler::optimize::PatternKind;

        match pattern.kind {
            PatternKind::Wildcard => result.push(Stmt::Expr(value)),
            PatternKind::Variable(var) => {
                result.push(Stmt::Var(vec![VarDecl {
                    id: Pat::Ident(mangle_variable(var)),
                    init: Some(value),
                }]));
            }
            PatternKind::Destructure(fields) => {
                for (index, pattern) in fields {
                    compile_pattern(
                        pattern,
                        Expr::Member(MemberExpr {
                            object: Box::new(value.clone()),
                            property: Box::new(Expr::Lit(Lit::Number(Cow::Owned(
                                index.to_string(),
                            )))),
                            computed: true,
                        }),
                        result,
                    );
                }
            }
            PatternKind::Variant(_, values) => {
                for pattern in values {
                    compile_pattern(
                        pattern,
                        Expr::Member(MemberExpr {
                            object: Box::new(value.clone()),
                            property: Box::new(Expr::Lit(Lit::String(
                                resast::expr::StringLit::Double(Cow::Borrowed(
                                    mangle_variant_values(),
                                )),
                            ))),
                            computed: false,
                        }),
                        result,
                    )
                }
            }
        }
    }

    let mut result = Vec::new();
    compile_pattern(pattern, value, &mut result);
    result
}

fn compile_statement(statement: wipple_compiler::optimize::Expression) -> Vec<Stmt<'static>> {
    use wipple_compiler::optimize::ExpressionKind;

    match statement.kind {
        ExpressionKind::Initialize(pattern, value) => {
            compile_pattern(pattern, compile_expression(*value))
        }
        _ => vec![Stmt::Expr(compile_expression(statement))],
    }
}

fn compile_expression(expr: wipple_compiler::optimize::Expression) -> Expr<'static> {
    use wipple_compiler::optimize::ExpressionKind;

    match expr.kind {
        ExpressionKind::Marker => Expr::Lit(Lit::Null),
        ExpressionKind::Variable(var) => Expr::Ident(mangle_variable(var)),
        ExpressionKind::Constant(constant) => Expr::Ident(mangle_constant(constant)),
        ExpressionKind::Text(text) => {
            Expr::Lit(Lit::String(StringLit::Double(Cow::Owned(text.to_string()))))
        }
        ExpressionKind::Number(number) => {
            // TODO: Decimal support
            Expr::Lit(Lit::Number(Cow::Owned(number.to_string())))
        }
        ExpressionKind::Block(mut statements) => {
            let mut tail = statements
                .pop()
                .map(compile_statement)
                .into_iter()
                .flatten()
                .collect::<Vec<_>>();

            if let Some(tail) = tail.last_mut() {
                replace_with::replace_with_or_abort(tail, |tail| match tail {
                    Stmt::Expr(expr) => Stmt::Return(Some(expr)),
                    _ => tail,
                });
            }

            let statements = statements
                .into_iter()
                .flat_map(compile_statement)
                .chain(tail)
                .map(ProgramPart::Stmt)
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
        ExpressionKind::Call(func, input) => Expr::Call(CallExpr {
            callee: Box::new(compile_expression(*func)),
            arguments: vec![compile_expression(*input)],
        }),
        ExpressionKind::Function(pattern, body, _) => Expr::ArrowFunc(ArrowFuncExpr {
            id: None,
            params: vec![FuncArg::Pat(Pat::Ident(mangle_function_input()))],
            body: ArrowFuncBody::FuncBody(FuncBody(
                compile_pattern(pattern, Expr::Ident(mangle_function_input()))
                    .into_iter()
                    .map(ProgramPart::Stmt)
                    .chain(std::iter::once(ProgramPart::Stmt(Stmt::Expr(
                        compile_expression(*body),
                    ))))
                    .collect(),
            )),
            expression: true,
            generator: false,
            is_async: false,
        }),
        ExpressionKind::When(_, _) => todo!(),
        ExpressionKind::External(namespace, identifier, inputs, _) => Expr::Call(CallExpr {
            callee: Box::new(Expr::Member(MemberExpr {
                object: Box::new(Expr::Member(MemberExpr {
                    object: Box::new(Expr::Ident(mangle_externals())),
                    property: Box::new(Expr::Lit(Lit::String(StringLit::Double(Cow::Owned(
                        namespace.to_string(),
                    ))))),
                    computed: true,
                })),
                property: Box::new(Expr::Lit(Lit::String(StringLit::Double(Cow::Owned(
                    identifier.to_string(),
                ))))),
                computed: true,
            })),
            arguments: inputs
                .into_iter()
                .map(|(expr, _)| compile_expression(expr))
                .collect(),
        }),
        ExpressionKind::Initialize(_, _) => {
            unreachable!("assignment is handed by the `Block` arm")
        }
        ExpressionKind::Structure(values) | ExpressionKind::ListLiteral(values) => Expr::Array(
            values
                .into_iter()
                .map(compile_expression)
                .map(Some)
                .collect(),
        ),
        ExpressionKind::Variant(index, values) => Expr::Array(
            std::iter::once(Expr::Lit(Lit::Number(Cow::Owned(index.to_string()))))
                .chain(values.into_iter().map(compile_expression))
                .map(Some)
                .collect(),
        ),
    }
}

fn mangle_constant(constant: wipple_compiler::MonomorphizedConstantId) -> Ident<'static> {
    Ident::new(format!("$wippleConst{}", constant.0))
}

fn mangle_variable(variable: wipple_compiler::VariableId) -> Ident<'static> {
    Ident::new(format!("$wippleVar{}", variable.0))
}

fn mangle_function_input() -> Ident<'static> {
    Ident::from("$wippleInput")
}

fn mangle_externals() -> Ident<'static> {
    Ident::from("$wippleExternals")
}

fn mangle_variant() -> &'static str {
    "$wippleVariant"
}

fn mangle_variant_values() -> &'static str {
    "$wippleValues"
}
