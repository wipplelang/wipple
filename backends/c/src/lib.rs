use std::{
    collections::{BTreeMap, HashMap},
    fmt::Display,
};
use wipple_frontend::ir;

pub fn generate(program: &ir::Program) -> String {
    let mut code = String::from(include_str!("../runtime/runtime.c"));

    let mut func_idents = BTreeMap::new();
    for label in 0..program.labels.len() {
        let id = func_idents.len();
        func_idents.insert(label, func_ident(id));
    }

    for (label, (kind, vars, blocks)) in program.labels.iter().enumerate() {
        let (param_tys, return_ty) = match kind {
            ir::LabelKind::Entrypoint => (Vec::new(), String::from("void")),
            ir::LabelKind::Constant(_) => todo!(),
            ir::LabelKind::Function(input_ty, output_ty) => {
                (vec![generate_ty(input_ty)], generate_ty(output_ty))
            }
            ir::LabelKind::Closure(_, _, _) => todo!(),
        };

        code.push('\n');
        code.push_str(&return_ty);
        code.push(' ');
        code.push_str(func_idents.get(&label).unwrap());
        code.push('(');

        for (index, ty) in param_tys.into_iter().enumerate() {
            if index != 0 {
                code.push_str(", ");
            }

            code.push_str(&ty);
        }

        code.push_str(") {\n");

        let mut intermediates = Vec::<&ir::Type>::new();
        macro_rules! new {
            ($ty:expr) => {{
                let var = intermediates.len();
                intermediates.push($ty);
                var
            }};
        }

        let mut variable_map = HashMap::new();
        let mut body = String::new();
        let mut stack = Vec::<usize>::new();
        for (index, block) in blocks.iter().enumerate() {
            body.push('\t');
            body.push_str(&block_ident(index));
            body.push_str(":\n");

            for statement in &block.statements {
                body.push_str("\t\t");

                match statement {
                    ir::Statement::Copy => {
                        let var = stack.last().unwrap();
                        let ty = intermediates[*var];
                        let new = new!(ty);
                        body.push_str(&var_ident(*var));
                        body.push_str(" = ");
                        body.push_str(&var_ident(new));

                        if ty.is_reference() {
                            body.push_str(";\n\t\t");
                            generate_increment_reference(new);
                        }

                        stack.push(new);
                    }
                    ir::Statement::Drop => {
                        let var = stack.pop().unwrap();
                        let ty = intermediates[var];
                        if ty.is_reference() {
                            generate_decrement_reference(var);
                        }
                    }
                    ir::Statement::Initialize(variable) => {
                        let var = stack.pop().unwrap();
                        let id = variable_map.len();
                        variable_map.insert(variable, id);
                        body.push_str(&var_ident(id));
                        body.push_str(" = ");
                        body.push_str(&var_ident(var));
                    }
                    ir::Statement::Free(_) => {
                        // TODO: Decrement reference count
                        todo!();
                    }
                    ir::Statement::Unpack(_) => todo!(),
                    ir::Statement::Expression(ty, expr) => {
                        let var = new!(ty);

                        body.push_str(&var_ident(var));
                        body.push_str(" = ");

                        match expr {
                            ir::Expression::Marker => {
                                body.push_str("struct ");
                                body.push_str(&marker_ty_ident());
                                body.push_str(" {}");
                            }
                            ir::Expression::Text(_) => todo!(),
                            ir::Expression::Number(_) => todo!(),
                            ir::Expression::Integer(integer) => {
                                body.push_str(&integer.to_string());
                            }
                            ir::Expression::Natural(natural) => {
                                body.push_str(&natural.to_string());
                            }
                            ir::Expression::Byte(byte) => {
                                body.push_str(&byte.to_string());
                            }
                            ir::Expression::Signed(signed) => {
                                body.push_str(&signed.to_string());
                            }
                            ir::Expression::Unsigned(unsigned) => {
                                body.push_str(&unsigned.to_string());
                            }
                            ir::Expression::Float(float) => {
                                body.push_str(&float.to_string());
                                body.push('f');
                            }
                            ir::Expression::Double(double) => {
                                body.push_str(&double.to_string());
                            }
                            ir::Expression::Variable(_) => todo!(),
                            ir::Expression::Constant(_) => todo!(),
                            ir::Expression::Function(_) => todo!(),
                            ir::Expression::Closure(_, _) => todo!(),
                            ir::Expression::Call => todo!(),
                            ir::Expression::External(_, _, _) => todo!(),
                            ir::Expression::Tuple(_) => todo!(),
                            ir::Expression::Structure(_) => todo!(),
                            ir::Expression::Variant(_, _) => todo!(),
                            ir::Expression::TupleElement(_) => todo!(),
                            ir::Expression::StructureElement(_) => todo!(),
                            ir::Expression::VariantElement(_, _) => todo!(),
                            ir::Expression::Reference => todo!(),
                            ir::Expression::Dereference => todo!(),
                        }

                        stack.push(var);
                    }
                }

                body.push_str(";\n");
            }

            body.push_str("\t\t");

            match &block.terminator {
                ir::Terminator::Return => {
                    let var = stack.pop().unwrap();
                    assert!(stack.is_empty());
                    body.push_str("return ");
                    body.push_str(&var_ident(var));
                }
                ir::Terminator::Jump(block) => {
                    body.push_str("goto ");
                    body.push_str(&block_ident(*block));
                }
                ir::Terminator::If(_, _, _) => todo!(),
            }

            body.push_str(";\n");
        }

        for (var, ty) in intermediates.into_iter().enumerate() {
            code.push('\t');
            code.push_str(&generate_ty(ty));
            code.push(' ');
            code.push_str(&var_ident(var));
            code.push_str(";\n")
        }

        code.push_str(&body);

        code.push_str("}\n");
    }

    code
}

fn generate_ty(ty: &ir::Type) -> String {
    match ty {
        ir::Type::Unreachable => String::from("void"),
        ir::Type::Marker => String::from("struct {}"),
        ir::Type::Number => todo!(),
        ir::Type::Integer => String::from("int64_t"),
        ir::Type::Natural => String::from("uint64_t"),
        ir::Type::Byte => String::from("int8_t"),
        ir::Type::Signed => String::from("int"),
        ir::Type::Unsigned => String::from("unsigned int"),
        ir::Type::Float => String::from("float"),
        ir::Type::Double => String::from("double"),
        ir::Type::Tuple(_) => todo!(),
        ir::Type::Structure(_) => todo!(),
        ir::Type::Enumeration(_) => todo!(),
        ir::Type::TextReference
        | ir::Type::ListReference(_)
        | ir::Type::MutableReference(_)
        | ir::Type::FunctionReference(_, _)
        | ir::Type::StructureReference(_)
        | ir::Type::EnumerationReference(_) => reference_ty_ident(),
    }
}

fn generate_increment_reference(var: usize) -> String {
    format!("__wipple_runtime__incref({})", var_ident(var))
}

fn generate_decrement_reference(var: usize) -> String {
    format!("__wipple_runtime__decref({})", var_ident(var))
}

fn block_ident(block: usize) -> String {
    id_ident("block", block)
}

fn func_ident(func: usize) -> String {
    id_ident("func", func)
}

fn var_ident(var: usize) -> String {
    id_ident("var", var)
}

fn ty_ident(ty: usize) -> String {
    id_ident("ty", ty)
}

fn captures_ident() -> String {
    ident("captures")
}

fn marker_ty_ident() -> String {
    ident("ty_marker")
}

fn reference_ty_ident() -> String {
    ident("ty_reference")
}

fn id_ident(prefix: &str, counter: usize) -> String {
    ident(format!("{}_{}", prefix, counter))
}

fn ident(s: impl Display) -> String {
    format!("__wipple__{}", s)
}
