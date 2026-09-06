mod program;

pub use program::*;

use crate::{
    codegen::{
        CodegenError,
        hir::{self},
    },
    db::{Db, Node},
    facts::Syntax,
    span::Span,
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

const FALSE_VARIANT: usize = 0;
const TRUE_VARIANT: usize = 1;
const IS_LESS_THAN_VARIANT: usize = 0;
const IS_EQUAL_VARIANT: usize = 1;
const IS_GREATER_THAN_VARIANT: usize = 2;
const BREAK_VARIANT: usize = 1;

impl<'a> Program {
    pub fn from_hir(
        db: &Db,
        program: &'a hir::Program,
        options: Options<'_>,
    ) -> Result<Self, CodegenError> {
        let mut writer = Writer {
            db,
            options,
            program: Default::default(),
            map: Default::default(),
        };

        for &file in &program.source_files {
            if let Some(Syntax(syntax)) = db.get(file) {
                let span = syntax.get(db).span(db);
                writer.program.source_files.push(span.clone());
            }
        }

        for key in program.definitions.keys() {
            let index = FunctionIndex(writer.map.functions.len());
            writer.map.functions.insert(key, index);
        }

        for (definition, function) in &program.definitions {
            let Some(&index) = writer.map.functions.get(definition) else {
                continue;
            };

            let function = writer.function(definition, function)?;

            writer.program.functions.insert(index, function);

            if let hir::DefinitionKey::TopLevel = definition {
                writer.program.main = Some(index);
            }
        }

        Ok(writer.program)
    }
}

#[derive(Debug)]
struct Writer<'a> {
    db: &'a Db,
    options: Options<'a>,
    program: Program,
    map: IndexMap<'a>,
}

impl<'a> Writer<'a> {
    fn function(
        &mut self,
        definition: &'a hir::DefinitionKey,
        function: &'a hir::Function,
    ) -> Result<Function, CodegenError> {
        let type_parameters = function
            .type_parameters
            .iter()
            .enumerate()
            .map(|(index, &node)| {
                let index = TyParameterIndex(index);
                self.map.ty_parameters.insert(node, index);
                Ok((index, TyParameter {}))
            })
            .collect::<Result<BTreeMap<_, _>, CodegenError>>()?;

        let offset = self.map.locals.entry(definition).or_default().len();

        let inputs = function
            .bounds
            .iter()
            .flatten()
            .chain(&function.inputs)
            .enumerate()
            .map(|(index, &input)| {
                let ty = self.ty(&hir::type_of(self.db, input)?)?;
                let mutable = self.db.contains::<IsMutated>(input);

                let index = LocalIndex(offset + index);

                self.map
                    .locals
                    .entry(definition)
                    .or_default()
                    .insert(input, index);

                Ok((index, Local { ty, mutable }))
            })
            .collect::<Result<BTreeMap<_, _>, CodegenError>>()?;

        let mut locals = BTreeMap::new();
        for instruction in &function.instructions {
            instruction.clone().for_each_node(&mut |node| {
                if self.map.local_index(definition, node).is_err() {
                    let ty = self.ty(&hir::type_of(self.db, node)?)?;
                    let mutable = self.db.contains::<IsMutated>(node);

                    let index = LocalIndex(offset + inputs.len() + locals.len());
                    locals.insert(index, Local { ty, mutable });
                    self.map
                        .locals
                        .entry(definition)
                        .or_default()
                        .insert(node, index);
                }

                Ok::<_, CodegenError>(())
            })?;
        }

        let body = self.instructions(definition, &function.instructions)?;

        Ok(Function {
            type_parameters,
            inputs,
            locals,
            body,
        })
    }

    fn instructions(
        &mut self,
        definition: &'a hir::DefinitionKey,
        instructions: &'a [hir::Instruction],
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
                        value: self.map.local_index(definition, *value)?,
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
                                variant: BREAK_VARIANT,
                            },
                            vec![
                                Statement::Assign {
                                    local: self.map.local_index(definition, *node)?,
                                    value: SourceMapped {
                                        node: Some(*node),
                                        inner: Expression::VariantElement {
                                            value: self.map.local_index(definition, *result)?,
                                            variant: BREAK_VARIANT,
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
                        statements.push(Statement::Trace {
                            trace: format_trace(span),
                        });
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
        definition: &'a hir::DefinitionKey,
        conditions: &'a [hir::Condition],
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
                                        true_variant: TRUE_VARIANT,
                                        false_variant: FALSE_VARIANT,
                                    },
                                },
                            ),
                            variant: TRUE_VARIANT,
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
                                        true_variant: TRUE_VARIANT,
                                        false_variant: FALSE_VARIANT,
                                    },
                                },
                            ),
                            variant: TRUE_VARIANT,
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
        definition: &'a hir::DefinitionKey,
        value: &'a hir::Value,
    ) -> Result<Expression, CodegenError> {
        Ok(match value {
            hir::Value::Bound(bound_path) => Expression::Bound {
                local: self.bound(definition, bound_path)?,
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
            } => self.constant(definition, constant_definition, bounds)?,
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
        definition: &'a hir::DefinitionKey,
        constant_definition: &'a hir::DefinitionKey,
        bounds: &'a BTreeMap<hir::BoundPath, hir::Instance>,
    ) -> Result<Expression, CodegenError> {
        Ok(Expression::Function {
            index: self.map.function_index(constant_definition)?,
            bounds: bounds
                .values()
                .map(|instance| {
                    Ok(match instance {
                        hir::Instance::Bound(bound_path) => {
                            let local = self.bound(definition, bound_path)?;
                            SourceMapped::new(None, Expression::Bound { local })
                        }
                        hir::Instance::Instance {
                            definition: instance_definition,
                            bounds,
                        } => SourceMapped::new(
                            None,
                            self.constant(definition, instance_definition, bounds)?,
                        ),
                    })
                })
                .collect::<Result<Vec<_>, CodegenError>>()?,
        })
    }

    fn bound(
        &mut self,
        definition: &'a hir::DefinitionKey,
        bound_path: &'a hir::BoundPath,
    ) -> Result<LocalIndex, CodegenError> {
        let [node] = bound_path.as_slice() else {
            return Err(anyhow::format_err!("bound {bound_path:?} not resolved"));
        };

        self.map.local_index(definition, *node)
    }

    fn intrinsic(
        &mut self,
        name: &str,
        inputs: &[LocalIndex],
    ) -> Result<Intrinsic<SourceMapped<Box<Expression>>>, CodegenError> {
        let local = |index: LocalIndex| {
            SourceMapped::new(None, Box::new(Expression::Local { local: index }))
        };

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
                true_variant: TRUE_VARIANT,
                false_variant: FALSE_VARIANT,
            },
            "number-equal" => NumberEqual(left, right) {
                true_variant: TRUE_VARIANT,
                false_variant: FALSE_VARIANT,
            },
            "order" => Order(left, right) {
                is_less_than_variant: IS_LESS_THAN_VARIANT,
                is_equal_variant: IS_EQUAL_VARIANT,
                is_greater_than_variant: IS_GREATER_THAN_VARIANT,
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
                true_variant: TRUE_VARIANT,
                false_variant: FALSE_VARIANT,
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
    ) -> Result<NamedTyIndex, CodegenError> {
        if let Some(index) = self.map.named_tys.get(&definition) {
            return Ok(*index);
        }

        let index = NamedTyIndex(self.map.named_tys.len());
        self.map.named_tys.insert(definition, index);

        let (parameters, representation) = hir::representation(self.db, definition, parameters)?;

        let parameters = parameters
            .into_iter()
            .enumerate()
            .map(|(index, node)| {
                self.map.ty_parameters.insert(node, TyParameterIndex(index));

                Ok(TyParameter {})
            })
            .collect::<Result<Vec<_>, CodegenError>>()?;

        let representation = match representation {
            hir::TypeRepresentation::Intrinsic(representation) => {
                TyRepresentation::Intrinsic(representation)
            }
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

        self.program.named_tys.insert(
            index,
            NamedTy {
                parameters,
                representation,
            },
        );

        Ok(index)
    }
}

#[derive(Debug, Default)]
struct IndexMap<'a> {
    functions: BTreeMap<&'a hir::DefinitionKey, FunctionIndex>,
    named_tys: BTreeMap<Node, NamedTyIndex>,
    ty_parameters: BTreeMap<Node, TyParameterIndex>,
    locals: BTreeMap<&'a hir::DefinitionKey, BTreeMap<Node, LocalIndex>>,
}

impl<'a> IndexMap<'a> {
    fn function_index(
        &self,
        definition: &'a hir::DefinitionKey,
    ) -> Result<FunctionIndex, CodegenError> {
        self.functions
            .get(definition)
            .copied()
            .ok_or_else(|| anyhow::format_err!("missing function {definition:?}"))
    }

    fn ty_parameter_index(&self, node: Node) -> Result<TyParameterIndex, CodegenError> {
        self.ty_parameters
            .get(&node)
            .copied()
            .ok_or_else(|| anyhow::format_err!("missing type parameter {node:?}"))
    }

    fn local_index(
        &self,
        definition: &hir::DefinitionKey,
        node: Node,
    ) -> Result<LocalIndex, CodegenError> {
        self.locals
            .get(definition)
            .and_then(|locals| locals.get(&node).copied())
            .ok_or_else(|| anyhow::format_err!("missing local {node:?}"))
    }
}

fn format_trace(span: &Span) -> serde_json::Value {
    serde_json::json!({
        "path": span.path,
        "start": span.start,
        "end": span.end,
    })
}
