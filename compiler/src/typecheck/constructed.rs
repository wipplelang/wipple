use crate::{
    database::{Db, NodeRef},
    nodes::TypeParameterNode,
    typecheck::{ConstructedType, ConstructedTypeTag, Type},
    visit::Defined,
};
use std::fmt::Write;

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
            move |_, children, root| {
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
            move |_, children, _| match children.as_slice() {
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
            move |_, children, _| {
                let output = children.into_iter().next().unwrap();
                format!("{{{}}}", output(true))
            },
        )
    }

    pub fn parameter_type(&self, definition: NodeRef) -> ConstructedType {
        let name = definition
            .downcast_ref::<TypeParameterNode>()
            .expect("not a type parameter")
            .name
            .clone();

        ConstructedType::new(
            ConstructedTypeTag::Parameter(definition.clone()),
            Vec::new(),
            move |_, _, _| name.clone(),
        )
        .instantiate(definition)
    }
}
