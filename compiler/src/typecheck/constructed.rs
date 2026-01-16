use crate::{
    database::{Db, NodeRef},
    nodes::TypeParameterNode,
    typecheck::{ConstructedType, ConstructedTypeInfo, ConstructedTypeTag, Type},
    visit::Defined,
};
use std::{fmt::Write, sync::Arc};

impl Db {
    pub fn named_type<T>(
        &self,
        definition: NodeRef,
        parameters: impl IntoIterator<Item = T>,
    ) -> ConstructedType
    where
        Type: From<T>,
    {
        ConstructedType::new(
            ConstructedTypeTag::Named(definition.clone()),
            parameters.into_iter().map(Type::from).collect::<Vec<_>>(),
            ConstructedTypeInfo {
                display: Arc::new({
                    let definition = definition.clone();
                    move |db, children, root| {
                        let wrap = !root && !children.is_empty();

                        let type_name = db
                            .get::<Defined>(&definition)
                            .expect("not a definition")
                            .0
                            .name()
                            .expect("not a type definition")
                            .to_string();

                        let mut result = type_name;

                        for child in children {
                            write!(&mut result, " {}", child(false)).unwrap();
                        }

                        if wrap {
                            format!("({})", result)
                        } else {
                            result
                        }
                    }
                }),
                serialize: Arc::new(move |children, node| {
                    serde_json::json!({
                        "__wipple_type": "named",
                        "__wipple_name": node(definition.clone()),
                        "__wipple_parameters": children,
                    })
                }),
            },
        )
    }

    pub fn function_type<T>(
        &self,
        inputs: impl IntoIterator<Item = T>,
        output: T,
    ) -> ConstructedType
    where
        Type: From<T>,
    {
        let children = [output]
            .into_iter()
            .chain(inputs)
            .map(Type::from)
            .collect::<Vec<_>>();

        ConstructedType::new(
            ConstructedTypeTag::Function,
            children,
            ConstructedTypeInfo {
                display: Arc::new(move |_db, children, root| {
                    let (output, inputs) = children.split_first().unwrap();

                    let mut result = String::new();
                    for input in inputs {
                        write!(&mut result, "{} ", input(false)).unwrap();
                    }

                    write!(&mut result, "-> {}", output(true)).unwrap();

                    if root {
                        result
                    } else {
                        format!("({})", result)
                    }
                }),
                serialize: Arc::new(move |children, _node| {
                    let (output, inputs) = children.split_first().unwrap();

                    serde_json::json!({
                        "__wipple_type": "function",
                        "__wipple_inputs": inputs,
                        "__wipple_output": output,
                    })
                }),
            },
        )
    }

    pub fn tuple_type<T>(&self, elements: impl IntoIterator<Item = T>) -> ConstructedType
    where
        Type: From<T>,
    {
        ConstructedType::new(
            ConstructedTypeTag::Tuple,
            elements.into_iter().map(Type::from).collect::<Vec<_>>(),
            ConstructedTypeInfo {
                display: Arc::new(move |_db, children, _root| match children.as_slice() {
                    [] => String::from("()"),
                    [first] => format!("({};)", first(true)),
                    children => {
                        let mut result = String::new();
                        for (index, child) in children.iter().enumerate() {
                            if index > 0 {
                                write!(&mut result, "; ").unwrap();
                            }

                            write!(&mut result, "{}", child(true)).unwrap();
                        }

                        format!("({})", result)
                    }
                }),
                serialize: Arc::new(move |children, _node| {
                    serde_json::json!({
                        "__wipple_type": "tuple",
                        "__wipple_elements": children,
                    })
                }),
            },
        )
    }

    pub fn unit_type(&self) -> ConstructedType {
        self.tuple_type::<Type>([])
    }

    pub fn block_type<T>(&self, output: T) -> ConstructedType
    where
        Type: From<T>,
    {
        ConstructedType::new(
            ConstructedTypeTag::Block,
            vec![Type::from(output)],
            ConstructedTypeInfo {
                display: Arc::new(move |_db, children, _root| {
                    let output = children.into_iter().next().unwrap();
                    format!("{{{}}}", output(true))
                }),
                serialize: Arc::new(move |children, _node| {
                    let output = children.into_iter().next().unwrap();
                    serde_json::json!({
                        "__wipple_type": "block",
                        "__wipple_output": output,
                    })
                }),
            },
        )
    }

    pub fn parameter_type(&self, definition: NodeRef) -> ConstructedType {
        ConstructedType::new(
            ConstructedTypeTag::Parameter(definition.clone()),
            Vec::new(),
            ConstructedTypeInfo {
                display: Arc::new({
                    let definition = definition.clone();
                    move |_db, _, _| {
                        definition
                            .downcast_ref::<TypeParameterNode>()
                            .expect("not a type parameter")
                            .name
                            .clone()
                    }
                }),
                serialize: Arc::new({
                    let definition = definition.clone();
                    move |_, node| {
                        serde_json::json!({
                            "__wipple_type": "parameter",
                            "__wipple_name": node(definition.clone()),
                        })
                    }
                }),
            },
        )
        .instantiate(definition)
    }
}
