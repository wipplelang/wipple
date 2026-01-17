use crate::{
    database::{Db, NodeRef},
    typecheck::{Instances, Type, Typed, group_instances},
    visit::{Defined, Definition},
};
use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone)]
pub struct CodegenError(String);

impl CodegenError {
    pub fn new(message: impl Into<String>) -> Self {
        CodegenError(message.into())
    }
}

impl Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for CodegenError {}

pub trait Codegen {
    fn codegen(&self, codegen: &mut CodegenCtx<'_>) -> Result<(), CodegenError>;

    fn identifier(&self) -> Option<String> {
        None
    }
}

#[derive(Debug, Clone)]
pub struct Options<'a> {
    pub prelude: &'a str,
    pub module: bool,
}

#[derive(Debug, Clone)]
struct WrittenType {
    index: usize,
    type_string: String,
}

pub struct CodegenCtx<'a> {
    pub db: &'a mut Db,
    options: Options<'a>,
    output: String,
    line: usize,
    column: usize,
    node: Option<NodeRef>,
    identifier: Option<String>,
    written_types: HashMap<String, WrittenType>,
}

const START_LINE: usize = 1;
const START_COLUMN: usize = 0;

pub fn codegen(
    db: &mut Db,
    files: &[NodeRef],
    options: Options<'_>,
) -> Result<String, CodegenError> {
    CodegenCtx::new(db, options).to_string(files)
}

impl<'a> CodegenCtx<'a> {
    fn new(db: &'a mut Db, options: Options<'a>) -> Self {
        CodegenCtx {
            db,
            options,
            output: String::new(),
            line: START_LINE,
            column: START_COLUMN,
            identifier: None,
            node: None,
            written_types: Default::default(),
        }
    }

    pub fn current_node(&self) -> &NodeRef {
        self.node.as_ref().unwrap()
    }

    pub fn node(&mut self, node: &NodeRef) -> String {
        format!("_{}", node.id())
    }

    pub fn write_node(&mut self, node: &NodeRef) {
        let name = self.node(node);
        self.write_string(&name);
    }

    pub fn write_line(&mut self) {
        self.output.push('\n');
        self.line += 1;
        self.column = 0;
    }

    pub fn write_string(&mut self, s: impl AsRef<str>) {
        let s = s.as_ref();
        self.output.push_str(s);
        self.column += s.len();
    }

    pub fn write_type(&mut self, ty: &Type) -> Result<(), CodegenError> {
        let written_type = self.ty(ty, &|codegen, json| {
            let type_code = serde_json::to_string(&json).unwrap();

            let index = codegen.written_types.len();

            codegen
                .written_types
                .entry(type_code.clone())
                .or_insert(WrittenType {
                    index,
                    type_string: type_code,
                })
                .clone()
        })?;

        self.write_string(format!("__wipple_typeCache[{}]", written_type.index));

        Ok(())
    }

    pub fn write(&mut self, node: &NodeRef) -> Result<(), CodegenError> {
        let prev_node = self.node.clone();
        let prev_identifier = self.identifier.clone();
        self.node = Some(node.clone());
        self.identifier = node.identifier();

        let result = node.codegen(self);

        self.identifier = prev_identifier;
        self.node = prev_node;

        result
    }

    pub fn error(&self) -> CodegenError {
        CodegenError::new(format!("cannot codegen {:?}", self.node))
    }

    fn ty<T>(
        &mut self,
        ty: &Type,
        write: &dyn Fn(&mut Self, serde_json::Value) -> T,
    ) -> Result<T, CodegenError> {
        // Get the latest type
        let ty = if let Type::Node(node) = ty
            && let Some(Typed { group: Some(group) }) = self.db.get(node)
            && let Some(latest) = group.types.first()
        {
            Type::Constructed(latest.clone())
        } else {
            ty.clone()
        };

        let constructed = match ty {
            Type::Constructed(ty) => ty,
            Type::Node(node) => {
                return Err(CodegenError::new(format!("unresolved type: {:?}", node)));
            }
        };

        let children = constructed
            .children
            .into_iter()
            .map(|child| self.ty(&child, &|_, json| json))
            .collect::<Result<Vec<_>, _>>()?;

        let type_json = (constructed.info.serialize)(children, &mut |node| self.node(&node));

        Ok(write(self, type_json))
    }

    fn write_definitions(&mut self) -> Result<(), CodegenError> {
        for (node, Defined(definition)) in self.db.iter::<Defined>().collect::<Vec<_>>() {
            let body = match definition {
                Definition::Trait(definition) => {
                    self.write_instances(&definition.node)?;
                    continue;
                }
                Definition::Constant(definition) => {
                    if !definition.assigned {
                        continue;
                    }

                    definition.value
                }
                Definition::Instance(definition) => match definition.value {
                    Some(value) => value,
                    None => continue,
                },
                _ => continue,
            };

            self.write_string(format!("/**! {} */ ", self.db.span(&node)));
            self.write_string("async function ");
            self.write_node(&node);
            self.write_string("(__wipple_types) {");
            self.write_line();
            self.write_string("return ");
            self.write(&body)?;
            self.write_string(";");
            self.write_line();
            self.write_string("}");
            self.write_line();
        }

        Ok(())
    }

    fn write_instances(&mut self, trait_node: &NodeRef) -> Result<(), CodegenError> {
        let Instances(instances) = self.db.get::<Instances>(trait_node).unwrap_or_default();

        self.write_string("const ");
        self.write_node(trait_node);
        self.write_string(" = [");
        self.write_line();

        for instance in group_instances(instances).flat_map(|(instances, _)| instances) {
            if instance.error {
                continue;
            }

            let mut parameters = instance.substitutions.keys();
            parameters.sort_by_key(|parameter| self.db.span(parameter));

            self.write_string("[");
            self.write_node(&instance.node);
            self.write_string(", {");
            for parameter in parameters {
                let substitution = instance
                    .substitutions
                    .get(&parameter)
                    .expect("missing substitution");

                self.write_node(&parameter);
                self.write_string(": ");
                self.write_type(&substitution)?;
                self.write_string(", ");
            }

            self.write_string("}],");
            self.write_line();
        }

        self.write_string("];");
        self.write_line();

        Ok(())
    }

    pub fn to_string(mut self, files: &[NodeRef]) -> Result<String, CodegenError> {
        colored::control::set_override(false);

        if self.options.module {
            self.write_string("let __wipple_env, __wipple_proxy;");
            self.write_line();
            self.write_string("export default async function(env, proxy) {");
            self.write_line();
            self.write_string("__wipple_env = env;");
            self.write_line();
            self.write_string("__wipple_proxy = proxy;");
            self.write_line();
        }

        self.write_definitions()?;

        self.write_string("const __wipple_types = {};");
        self.write_line();

        for file in files {
            self.write(file)?;
        }

        if self.options.module {
            self.write_string("};\n");
        }

        let mut type_cache = vec![String::new(); self.written_types.len()];
        for written_type in self.written_types.into_values() {
            type_cache[written_type.index] = written_type.type_string;
        }

        let type_cache = format!(
            "const __wipple_typeCache = [\n{}];\n",
            type_cache.join(",\n")
        );

        let prelude = format!("{}{}", self.options.prelude, type_cache);

        let script = format!("{}{}", prelude, self.output);

        colored::control::unset_override();

        Ok(script)
    }
}
