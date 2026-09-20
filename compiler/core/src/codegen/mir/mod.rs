mod program;

pub use program::*;

use crate::{
    codegen::{
        CodegenError,
        hir::{self},
    },
    db::{Db, Node},
    facts::Syntax,
    visit::IsMutated,
};
use std::collections::BTreeMap;

#[derive(Debug, Clone, Copy, Default)]
pub struct Options<'a> {
    pub trace: TraceOptions<'a>,
}

#[derive(Debug, Clone, Copy, Default)]
pub enum TraceOptions<'a> {
    #[default]
    None,
    All,
    Files(&'a [&'a str]),
}

pub mod builtin_variants {
    pub const FALSE: usize = 0;
    pub const TRUE: usize = 1;
    pub const NONE: usize = 0;
    pub const SOME: usize = 1;
    pub const IS_LESS_THAN: usize = 0;
    pub const IS_EQUAL: usize = 1;
    pub const IS_GREATER_THAN: usize = 2;
    pub const BREAK: usize = 1;
}

impl Program {
    pub fn extend_from_hir(
        &mut self,
        db: &Db,
        program: &hir::Program,
        map: &mut IndexMap,
        options: Options<'_>,
    ) -> Result<(), CodegenError> {
        let mut writer = Writer {
            db,
            options,
            map,
            program: self,
        };

        for &file in &program.source_files {
            if let Some(Syntax(syntax)) = db.get(file) {
                let span = syntax.get(db).span(db);
                writer.program.source_files.push(span.clone());
            }
        }

        writer.program.functions.resize_with(
            writer.program.functions.len() + program.definitions.len(),
            Function::default,
        );

        for key in program.definitions.keys() {
            let index = writer.map.functions.len();
            writer.map.functions.insert(*key, index);
        }

        for (definition, function) in &program.definitions {
            let Some(&index) = writer.map.functions.get(definition) else {
                continue;
            };

            let function = writer.function(*definition, function)?;
            writer.program.functions[index] = function;

            if let hir::DefinitionKey::TopLevel = definition {
                writer.program.main = Some(index);
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
struct Writer<'a> {
    db: &'a Db,
    options: Options<'a>,
    map: &'a mut IndexMap,
    program: &'a mut Program,
}

impl Writer<'_> {
    fn function(
        &mut self,
        definition: hir::DefinitionKey,
        function: &hir::Function,
    ) -> Result<Function, CodegenError> {
        let type_parameters = function
            .type_parameters
            .iter()
            .enumerate()
            .map(|(index, &node)| {
                self.map.ty_parameters.insert(node, index);
                Ok(TyParameter {})
            })
            .collect::<Result<Vec<_>, CodegenError>>()?;

        let captures = function
            .captures
            .iter()
            .map(|&node| self.map.local_index(definition, node))
            .collect::<Result<Vec<_>, CodegenError>>()?;

        self.map
            .locals
            .entry(definition)
            .or_default()
            .push(Default::default());

        for (local, node) in function.captures.iter().enumerate() {
            self.map
                .locals
                .entry(definition)
                .or_default()
                .last_mut()
                .unwrap()
                .insert(*node, local);
        }

        let inputs = function
            .bounds
            .iter()
            .flatten()
            .chain(&function.inputs)
            .enumerate()
            .map(|(index, &input)| {
                let ty = self.ty(&hir::type_of(self.db, input)?)?;
                let mutable = self.db.contains::<IsMutated>(input);

                let index = captures.len() + index;

                self.map
                    .locals
                    .entry(definition)
                    .or_default()
                    .last_mut()
                    .unwrap()
                    .insert(input, index);

                Ok(Local { ty, mutable })
            })
            .collect::<Result<Vec<_>, CodegenError>>()?;

        let mut locals = Vec::new();
        for instruction in &function.instructions {
            instruction.clone().for_each_node(&mut |node| {
                if self.map.local_index(definition, node).is_err() {
                    let ty = self.ty(&hir::type_of(self.db, node)?)?;
                    let mutable = self.db.contains::<IsMutated>(node);

                    let index = captures.len() + inputs.len() + locals.len();
                    locals.push(Local { ty, mutable });

                    self.map
                        .locals
                        .entry(definition)
                        .or_default()
                        .last_mut()
                        .unwrap()
                        .insert(node, index);
                }

                Ok::<_, CodegenError>(())
            })?;
        }

        let body = self.instructions(definition, &function.instructions)?;

        self.map
            .locals
            .entry(definition)
            .or_default()
            .pop()
            .unwrap();

        Ok(Function {
            type_parameters,
            captures,
            inputs,
            locals,
            body,
        })
    }

    fn instructions(
        &mut self,
        definition: hir::DefinitionKey,
        instructions: &[hir::Instruction],
    ) -> Result<Vec<Statement>, CodegenError> {
        let mut statements = Vec::new();
        for instruction in instructions {
            match instruction {
                hir::Instruction::If {
                    node,
                    branches: hir_branches,
                    else_branch: hir_else_branch,
                } => {
                    let mut branches = Vec::new();
                    for (conditions, instructions, then_node) in hir_branches {
                        let conditions = self.condition(definition, conditions)?;

                        let mut instructions = self.instructions(definition, instructions)?;
                        if let Some(node) = *node
                            && let Some(then_node) = *then_node
                        {
                            instructions.push(Statement::Assign {
                                local: self.map.local_index(definition, node)?,
                                value: SourceMapped::new(
                                    Some(then_node),
                                    Expression::Local {
                                        local: self.map.local_index(definition, then_node)?,
                                    },
                                ),
                            });
                        }

                        branches.push((conditions, instructions));
                    }

                    let else_branch = match hir_else_branch {
                        Some((instructions, else_node)) => {
                            let mut instructions = self.instructions(definition, instructions)?;
                            if let Some(node) = *node
                                && let Some(else_node) = *else_node
                            {
                                instructions.push(Statement::Assign {
                                    local: self.map.local_index(definition, node)?,
                                    value: SourceMapped::new(
                                        Some(else_node),
                                        Expression::Local {
                                            local: self.map.local_index(definition, else_node)?,
                                        },
                                    ),
                                });
                            }

                            Some(instructions)
                        }
                        None => None,
                    };

                    statements.push(Statement::If {
                        branches,
                        else_branch,
                    });
                }
                hir::Instruction::Return { value } => {
                    statements.push(Statement::Return {
                        value: SourceMapped::new(
                            Some(*value),
                            Expression::Local {
                                local: self.map.local_index(definition, *value)?,
                            },
                        ),
                    });
                }
                hir::Instruction::Loop { node, body, result } => {
                    let mut body = self.instructions(definition, body)?;

                    body.push(Statement::If {
                        branches: vec![(
                            Condition::Variant {
                                value: SourceMapped::new(
                                    Some(*result),
                                    Expression::Local {
                                        local: self.map.local_index(definition, *result)?,
                                    },
                                ),
                                variant: builtin_variants::BREAK,
                            },
                            vec![
                                Statement::Assign {
                                    local: self.map.local_index(definition, *node)?,
                                    value: SourceMapped {
                                        node: Some(*node),
                                        inner: Expression::VariantElement {
                                            value: self.map.local_index(definition, *result)?,
                                            variant: builtin_variants::BREAK,
                                            index: 0,
                                        },
                                    },
                                },
                                Statement::Break,
                            ],
                        )],
                        else_branch: None,
                    });

                    statements.push(Statement::Loop { body });
                }
                hir::Instruction::Trace { span } => {
                    let can_trace = match self.options.trace {
                        TraceOptions::None => false,
                        TraceOptions::All => true,
                        TraceOptions::Files(files) => files.contains(&span.path.as_str()),
                    };

                    if can_trace {
                        statements.push(Statement::Trace { span: span.clone() });
                    }
                }
                hir::Instruction::Value { node, value } => statements.push(Statement::Assign {
                    local: self.map.local_index(definition, *node)?,
                    value: SourceMapped::new(Some(*node), self.expression(definition, value)?),
                }),
            }
        }

        Ok(statements)
    }

    fn condition(
        &mut self,
        definition: hir::DefinitionKey,
        conditions: &[hir::Condition],
    ) -> Result<Condition, CodegenError> {
        conditions
            .iter()
            .try_fold(Condition::True, |result, condition| {
                Ok(Condition::And {
                    left: Box::new(result),
                    right: Box::new(match condition {
                        hir::Condition::Or(branches) => {
                            branches
                                .iter()
                                .try_fold(Condition::False, |result, conditions| {
                                    Ok::<_, CodegenError>(Condition::Or {
                                        left: Box::new(result),
                                        right: Box::new(self.condition(definition, conditions)?),
                                    })
                                })?
                        }
                        hir::Condition::EqualToNumber { input, value } => Condition::Variant {
                            value: SourceMapped::new(
                                None,
                                Expression::Intrinsic {
                                    intrinsic: Intrinsic::NumberEqual {
                                        left: SourceMapped::new(
                                            None,
                                            Box::new(Expression::Local {
                                                local: self.map.local_index(definition, *input)?,
                                            }),
                                        ),
                                        right: SourceMapped::new(
                                            None,
                                            Box::new(Expression::Number {
                                                value: value.clone(),
                                            }),
                                        ),
                                        true_variant: builtin_variants::TRUE,
                                        false_variant: builtin_variants::FALSE,
                                    },
                                },
                            ),
                            variant: builtin_variants::TRUE,
                        },
                        hir::Condition::EqualToString { input, value } => Condition::Variant {
                            value: SourceMapped::new(
                                None,
                                Expression::Intrinsic {
                                    intrinsic: Intrinsic::StringEqual {
                                        left: SourceMapped::new(
                                            None,
                                            Box::new(Expression::Local {
                                                local: self.map.local_index(definition, *input)?,
                                            }),
                                        ),
                                        right: SourceMapped::new(
                                            None,
                                            Box::new(Expression::String {
                                                value: value.clone(),
                                            }),
                                        ),
                                        true_variant: builtin_variants::TRUE,
                                        false_variant: builtin_variants::FALSE,
                                    },
                                },
                            ),
                            variant: builtin_variants::TRUE,
                        },
                        hir::Condition::EqualToVariant {
                            input,
                            variant_index,
                            ..
                        } => Condition::Variant {
                            value: SourceMapped::new(
                                None,
                                Expression::Local {
                                    local: self.map.local_index(definition, *input)?,
                                },
                            ),
                            variant: *variant_index,
                        },
                        hir::Condition::Initialize {
                            variable,
                            node,
                            value,
                            ..
                        } => Condition::Initialize {
                            local: self.map.local_index(definition, *variable)?,
                            value: SourceMapped::new(*node, self.expression(definition, value)?),
                        },
                        hir::Condition::Mutate { input, variable } => Condition::Mutate {
                            local: self.map.local_index(definition, *variable)?,
                            value: self.map.local_index(definition, *input)?,
                        },
                    }),
                })
            })
    }

    fn expression(
        &mut self,
        definition: hir::DefinitionKey,
        value: &hir::Value,
    ) -> Result<Expression, CodegenError> {
        Ok(match value {
            hir::Value::Bound(node) => Expression::Local {
                local: self.bound(definition, *node)?,
            },
            hir::Value::Call { function, inputs } => Expression::Call {
                function: self.map.local_index(definition, *function)?,
                inputs: inputs
                    .iter()
                    .map(|&input| self.map.local_index(definition, input))
                    .collect::<Result<Vec<_>, CodegenError>>()?,
            },
            hir::Value::Constant {
                definition: constant_definition,
                bounds,
            } => self.constant(definition, *constant_definition, bounds)?,
            hir::Value::Function(function) => {
                Expression::Closure(self.function(definition, function)?)
            }
            hir::Value::Field {
                input, field_index, ..
            } => Expression::Element {
                value: self.map.local_index(definition, *input)?,
                index: *field_index,
            },
            hir::Value::Tuple(elements) => Expression::Tuple {
                elements: elements
                    .iter()
                    .map(|&node| self.map.local_index(definition, node))
                    .collect::<Result<_, CodegenError>>()?,
            },
            hir::Value::Marker => Expression::Marker,
            hir::Value::MutableVariable(node) => Expression::MutableLocal {
                local: self.map.local_index(definition, *node)?,
            },
            hir::Value::Number(value) => Expression::Number {
                value: value.clone(),
            },
            hir::Value::Runtime { name, inputs } => {
                let inputs = inputs
                    .iter()
                    .map(|&input| self.map.local_index(definition, input))
                    .collect::<Result<Vec<_>, CodegenError>>()?;

                Expression::Intrinsic {
                    intrinsic: self.intrinsic(name, &inputs)?,
                }
            }
            hir::Value::String(value) => Expression::String {
                value: value.clone(),
            },
            hir::Value::Structure(fields) => Expression::Structure {
                fields: fields
                    .iter()
                    .map(|&(index, _, node)| Ok((index, self.map.local_index(definition, node)?)))
                    .collect::<Result<_, CodegenError>>()?,
            },
            hir::Value::TupleElement { input, index } => Expression::Element {
                value: self.map.local_index(definition, *input)?,
                index: *index,
            },
            hir::Value::Variable(node) => Expression::Local {
                local: self.map.local_index(definition, *node)?,
            },
            hir::Value::Variant {
                index, elements, ..
            } => Expression::Variant {
                variant: *index,
                elements: elements
                    .iter()
                    .map(|&node| self.map.local_index(definition, node))
                    .collect::<Result<_, CodegenError>>()?,
            },
            hir::Value::VariantElement {
                input,
                variant_index,
                element,
                ..
            } => Expression::VariantElement {
                value: self.map.local_index(definition, *input)?,
                variant: *variant_index,
                index: *element,
            },
        })
    }

    fn constant(
        &mut self,
        definition: hir::DefinitionKey,
        constant_definition: hir::DefinitionKey,
        bounds: &BTreeMap<hir::BoundPath, Node>,
    ) -> Result<Expression, CodegenError> {
        Ok(Expression::Function {
            index: self.map.function_index(constant_definition)?,
            bounds: bounds
                .values()
                .map(|node| self.map.local_index(definition, *node))
                .collect::<Result<Vec<_>, CodegenError>>()?,
        })
    }

    fn bound(&mut self, definition: hir::DefinitionKey, node: Node) -> Result<usize, CodegenError> {
        self.map.local_index(definition, node)
    }

    fn intrinsic(
        &mut self,
        name: &str,
        inputs: &[usize],
    ) -> Result<Intrinsic<SourceMapped<Box<Expression>>>, CodegenError> {
        let local = |local| SourceMapped::new(None, Box::new(Expression::Local { local }));

        macro_rules! intrinsics {
            ($($s:literal => $name:ident($($arg:ident),* $(,)?) $({ $($t:tt)* })?),* $(,)?) => {
                match (name, inputs) {
                    $(
                        ($s, &[$($arg),*]) => Ok(Intrinsic::$name {
                            $($arg: local($arg),)*
                            $($($t)*)?
                        }),
                    )*
                    _ => Err(anyhow::format_err!(
                        "invalid intrinsic {name:?} with inputs {inputs:?}"
                    )),
                }
            };
        }

        intrinsics!(
            "debug" => Debug(value),
            "string-count" => StringCount(value),
            "string-concat" => StringConcat(left, right),
            "external" => External(name, value),
            "number-to-string" => NumberToString(value),
            "string-to-number" => StringToNumber(value),
            "add" => Add(left, right),
            "sub" => Sub(left, right),
            "mul" => Mul(left, right),
            "div" => Div(left, right),
            "rem" => Rem(left, right),
            "pow" => Pow(left, right),
            "floor" => Floor(value),
            "ceil" => Ceil(value),
            "sqrt" => Sqrt(value),
            "neg" => Neg(value),
            "sin" => Sin(value),
            "cos" => Cos(value),
            "tan" => Tan(value),
            "string-equal" => StringEqual(left, right) {
                true_variant: builtin_variants::TRUE,
                false_variant: builtin_variants::FALSE,
            },
            "number-equal" => NumberEqual(left, right) {
                true_variant: builtin_variants::TRUE,
                false_variant: builtin_variants::FALSE,
            },
            "order" => Order(left, right) {
                is_less_than_variant: builtin_variants::IS_LESS_THAN,
                is_equal_variant: builtin_variants::IS_EQUAL,
                is_greater_than_variant: builtin_variants::IS_GREATER_THAN,
            },
            "empty-list" => EmptyList(),
            "list-count" => ListCount(value),
            "list-first" => ListFirst(value),
            "list-last" => ListLast(value),
            "list-initial" => ListInitial(value),
            "list-tail" => ListTail(value),
            "list-nth" => ListNth(value, index),
            "list-append" => ListAppend(value, element),
            "list-prepend" => ListPrepend(value, element),
            "list-insert-at" => ListInsertAt(value, index, element),
            "list-remove-at" => ListRemoveAt(value, index),
            "string-characters" => StringCharacters(value),
            "random-number" => RandomNumber(min, max),
            "nan" => Nan(),
            "is-nan" => IsNan(value) {
                true_variant: builtin_variants::TRUE,
                false_variant: builtin_variants::FALSE,
            },
            "hash-string" => HashString(value),
            "unreachable" => Unreachable(),
        )
    }

    fn ty(&mut self, ty: &hir::Type) -> Result<Ty, CodegenError> {
        match ty {
            hir::Type::Named {
                definition,
                parameters,
            } => Ok(Ty::Named {
                index: self.named_ty(*definition, parameters)?,
                parameters: parameters
                    .iter()
                    .map(|ty| self.ty(ty))
                    .collect::<Result<Vec<_>, _>>()?,
            }),
            hir::Type::Tuple(elements) => Ok(Ty::Tuple {
                elements: elements
                    .iter()
                    .map(|ty| self.ty(ty))
                    .collect::<Result<Vec<_>, _>>()?,
            }),
            hir::Type::Function(inputs, output) => Ok(Ty::Function {
                inputs: inputs
                    .iter()
                    .map(|ty| self.ty(ty))
                    .collect::<Result<Vec<_>, _>>()?,
                output: Box::new(self.ty(output)?),
            }),
            hir::Type::Parameter(node) => {
                let index = self.map.ty_parameter_index(*node)?;
                Ok(Ty::Parameter { index })
            }
        }
    }

    fn named_ty(
        &mut self,
        definition: Node,
        parameters: &[hir::Type],
    ) -> Result<usize, CodegenError> {
        if let Some(index) = self.map.named_tys.get(&definition) {
            return Ok(*index);
        }

        let index = self.map.named_tys.len();
        self.map.named_tys.insert(definition, index);

        let (parameters, representation) = hir::representation(self.db, definition, parameters)?;

        let parameters = parameters
            .into_iter()
            .enumerate()
            .map(|(index, node)| {
                self.map.ty_parameters.insert(node, index);
                Ok(TyParameter {})
            })
            .collect::<Result<Vec<_>, CodegenError>>()?;

        let representation = match representation {
            hir::TypeRepresentation::Intrinsic(representation) => match representation {
                hir::IntrinsicRepresentation::Opaque => TyRepresentation::Opaque,
                hir::IntrinsicRepresentation::Number => TyRepresentation::Number,
                hir::IntrinsicRepresentation::String => TyRepresentation::String,
            },
            hir::TypeRepresentation::Marker => TyRepresentation::Marker,
            hir::TypeRepresentation::Structure(fields) => TyRepresentation::Structure {
                fields: fields
                    .iter()
                    .map(|ty| self.ty(ty))
                    .collect::<Result<Vec<_>, _>>()?,
            },
            hir::TypeRepresentation::Enumeration(variants) => TyRepresentation::Enumeration {
                variants: variants
                    .iter()
                    .map(|elements| elements.iter().map(|ty| self.ty(ty)).collect())
                    .collect::<Result<Vec<_>, _>>()?,
            },
        };

        self.program.named_tys.push(NamedTy {
            parameters,
            representation,
        });

        Ok(index)
    }
}

#[derive(Debug, Default)]
pub struct IndexMap {
    functions: BTreeMap<hir::DefinitionKey, usize>,
    named_tys: BTreeMap<Node, usize>,
    ty_parameters: BTreeMap<Node, usize>,
    locals: BTreeMap<hir::DefinitionKey, Vec<BTreeMap<Node, usize>>>,
}

impl IndexMap {
    fn function_index(&self, definition: hir::DefinitionKey) -> Result<usize, CodegenError> {
        self.functions
            .get(&definition)
            .copied()
            .ok_or_else(|| anyhow::format_err!("missing function {definition:?}"))
    }

    fn ty_parameter_index(&self, node: Node) -> Result<usize, CodegenError> {
        self.ty_parameters
            .get(&node)
            .copied()
            .ok_or_else(|| anyhow::format_err!("missing type parameter {node:?}"))
    }

    fn local_index(
        &self,
        definition: hir::DefinitionKey,
        node: Node,
    ) -> Result<usize, CodegenError> {
        self.locals
            .get(&definition)
            .and_then(|locals| locals.last())
            .and_then(|locals| locals.get(&node).copied())
            .ok_or_else(|| anyhow::format_err!("missing local {node:?}"))
    }
}
