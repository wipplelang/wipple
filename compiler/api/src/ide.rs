use crate::CompileResult;
use std::collections::{BTreeMap, HashMap};
use wasm_bindgen::prelude::*;
use wipple_core::{
    db::Node,
    default_filter,
    facts::Syntax,
    render::RenderCtx,
    span::Span,
    typecheck::ty::Ty,
    visit::{
        Resolved,
        definitions::{
            ConstantDefinition, Defined, Definition, MarkerConstructorDefinition,
            StructureConstructorDefinition, TraitDefinition, TypeDefinition,
            TypeParameterDefinition, VariableDefinition, VariantConstructorDefinition,
        },
    },
};
use wipple_feedback::{FeedbackWriter, collect_feedback};
use wipple_syntax::{
    expressions::{
        constructor_expression::ConstructorExpression, variable_expression::VariableExpression,
    },
    patterns::{constructor_pattern::ConstructorPattern, variable_pattern::VariablePattern},
    types::named_type::NamedType,
};

#[wasm_bindgen(getter_with_clone, inspectable)]
#[derive(Debug, Clone)]
pub struct IdeDiagnostic {
    pub range: IdeRange,
    pub message: String,
}

#[wasm_bindgen(getter_with_clone, inspectable)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IdeRange {
    pub start: IdePosition,
    pub end: IdePosition,
}

#[wasm_bindgen(getter_with_clone, inspectable)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IdePosition {
    pub line: usize,
    pub column: usize,
}

#[wasm_bindgen(getter_with_clone, inspectable)]
#[derive(Debug, Clone)]
pub struct IdeSemanticToken {
    pub range: IdeRange,
    #[wasm_bindgen(js_name = "type")]
    pub ty: String,
}

#[wasm_bindgen(getter_with_clone, inspectable)]
#[derive(Debug, Clone)]
pub struct IdeHover {
    pub contents: Vec<IdeHoverItem>,
    pub range: IdeRange,
}

#[wasm_bindgen(getter_with_clone, inspectable)]
#[derive(Debug, Clone)]
pub struct IdeHoverItem {
    pub value: String,
    pub is_code: bool,
}

#[wasm_bindgen(getter_with_clone, inspectable)]
#[derive(Debug, Clone)]
pub struct IdeDefinition {
    pub name: String,
    #[wasm_bindgen(js_name = "type")]
    pub ty: String,
    pub definition: String,
    pub comments: Option<String>,
}

#[wasm_bindgen]
pub struct Ide {
    result: CompileResult,
}

#[wasm_bindgen]
impl Ide {
    #[wasm_bindgen(constructor)]
    pub fn new(result: CompileResult) -> Self {
        Ide { result }
    }

    pub fn diagnostics(&self) -> Vec<IdeDiagnostic> {
        let filter = default_filter;

        collect_feedback(&self.result.db, filter, |item| {
            filter(&self.result.db, item.location.primary)
        })
        .into_iter()
        .filter_map(|item| {
            let span = self
                .result
                .db
                .get::<Syntax>(item.location.primary)?
                .0
                .get(&self.result.db)
                .span(&self.result.db);

            let feedback = item.display(&self.result.db, |db, segment| segment.markdown(db, false));

            Some(IdeDiagnostic {
                range: span.into(),
                message: feedback.message,
            })
        })
        .collect()
    }

    pub fn semantic_tokens(&self) -> Vec<IdeSemanticToken> {
        let mut tokens = HashMap::<IdeRange, String>::new();
        for node in self.result.db.owned_nodes() {
            let Some(span) = self
                .result
                .db
                .get(node)
                .map(|Syntax(syntax)| syntax.get(&self.result.db).span(&self.result.db))
            else {
                continue;
            };

            let range = IdeRange::from(span);

            if wipple_queries::highlight_type(&self.result.db, node) {
                tokens.insert(range.clone(), String::from("type"));
            }

            if wipple_queries::highlight_trait(&self.result.db, node) {
                tokens.insert(range.clone(), String::from("interface"));
            }

            if wipple_queries::highlight_type_parameter(&self.result.db, node) {
                tokens.insert(range.clone(), String::from("typeParameter"));
            }

            if wipple_queries::highlight_function(&self.result.db, node) {
                tokens.insert(range, String::from("function"));
            }
        }

        tokens
            .into_iter()
            .map(|(range, ty)| IdeSemanticToken { range, ty })
            .collect()
    }

    pub fn hover(&self, line: usize, column: usize) -> Option<IdeHover> {
        let node = self.node_at(line, column)?;
        let span = self
            .result
            .db
            .get::<Syntax>(node)?
            .0
            .get(&self.result.db)
            .span(&self.result.db);

        let mut contents = Vec::new();

        let definition_node = self
            .result
            .db
            .get(node)
            .and_then(|Resolved { definitions, .. }| definitions.first().copied());

        if let Some(definition_node) = definition_node
            && let Some(Defined(definition)) = self.result.db.get(definition_node)
            && definition.downcast_ref::<VariableDefinition>().is_none()
        {
            let mut ctx = RenderCtx::with_filter(&default_filter);
            ctx.node(definition_node);

            let (rendered, _) = ctx.finish(&self.result.db, |db, segment| segment.plain_text(db));

            contents.push(IdeHoverItem {
                value: rendered.to_string(),
                is_code: true,
            });
        } else if let Some(ty) = wipple_queries::has_type(&self.result.db, node) {
            let mut ctx = RenderCtx::with_filter(&default_filter);

            if definition_node.is_some() {
                ctx.node(node);
                ctx.string(" :: ");
            }

            ctx.ty(&self.result.db, &Ty::Constructed(ty.clone()), None, true);

            let (rendered, _) = ctx.finish(&self.result.db, |db, segment| segment.plain_text(db));

            contents.push(IdeHoverItem {
                value: rendered.to_string(),
                is_code: true,
            });
        }

        for bound in wipple_queries::resolved_bounds(&self.result.db, node) {
            let mut ctx = RenderCtx::with_filter(&default_filter);
            ctx.node(bound.instance.node);

            let (rendered, _) = ctx.finish(&self.result.db, |db, segment| segment.plain_text(db));

            contents.push(IdeHoverItem {
                value: rendered.to_string(),
                is_code: true,
            });
        }

        if let Some(comments) = self.comments(node) {
            contents.push(IdeHoverItem {
                value: comments,
                is_code: false,
            });
        }

        (!contents.is_empty()).then(|| IdeHover {
            contents,
            range: span.into(),
        })
    }

    pub fn highlight(&self, line: usize, column: usize) -> Vec<IdeRange> {
        let Some(node) = self.node_at(line, column) else {
            return Vec::new();
        };

        wipple_queries::in_group(&self.result.db, node)
            .iter()
            .copied()
            .filter(|&related| default_filter(&self.result.db, related))
            .filter_map(|related| self.result.db.get(related))
            .map(|Syntax(syntax)| IdeRange::from(syntax.get(&self.result.db).span(&self.result.db)))
            .collect()
    }

    pub fn definition(&self, line: usize, column: usize) -> Option<IdeRange> {
        let node = self.node_at(line, column)?;

        let definition = wipple_queries::definitions(&self.result.db, node)?
            .first()
            .copied()?;

        if !default_filter(&self.result.db, definition) {
            return None;
        }

        self.result
            .db
            .get(definition)
            .map(|Syntax(syntax)| IdeRange::from(syntax.get(&self.result.db).span(&self.result.db)))
    }

    pub fn references(&self, line: usize, column: usize) -> Vec<IdeRange> {
        let Some(node) = self.node_at(line, column) else {
            return Vec::new();
        };

        let nodes = wipple_queries::definitions(&self.result.db, node)
            .map(|definitions| definitions.iter().copied())
            .unwrap_or_default();

        nodes
            .into_iter()
            .flat_map(|node| wipple_queries::references(&self.result.db, node))
            .filter(|&reference| default_filter(&self.result.db, reference))
            .filter_map(|reference| self.result.db.get(reference))
            .map(|Syntax(syntax)| IdeRange::from(syntax.get(&self.result.db).span(&self.result.db)))
            .collect()
    }

    pub fn autocomplete(&self, line: usize, column: usize) -> Vec<IdeDefinition> {
        let mut nodes = vec![self.result.root_node];

        let node_at_position = self.node_at(line, column);

        if let Some(node) = node_at_position {
            nodes.push(node);
        }

        let prefix = node_at_position
            .and_then(|node| {
                let syntax = self.result.db.get::<Syntax>(node)?.0.get(&self.result.db);

                let prefix = if let Some(value) = syntax.downcast_ref::<VariableExpression>() {
                    value.variable.to_string()
                } else if let Some(value) = syntax.downcast_ref::<VariablePattern>() {
                    value.variable.to_string()
                } else if let Some(value) = syntax.downcast_ref::<ConstructorExpression>() {
                    value.constructor.to_string()
                } else if let Some(value) = syntax.downcast_ref::<ConstructorPattern>() {
                    value.constructor.to_string()
                } else if let Some(value) = syntax.downcast_ref::<NamedType>() {
                    value.name.to_string()
                } else {
                    String::new()
                };

                let span = syntax.span(&self.result.db);

                // Ensure the cursor is actually within the prefix
                if span.end.column > span.start.column + prefix.len() {
                    return None;
                }

                Some(prefix)
            })
            .unwrap_or_default();

        let mut definitions = BTreeMap::<Node, &dyn Definition>::new();
        for node in nodes {
            for scope in wipple_queries::scopes(&self.result.db, node) {
                for (name, scope_definitions) in scope {
                    if !name.starts_with(prefix.as_str()) {
                        continue;
                    }

                    for (&node, definition) in scope_definitions {
                        definitions.insert(node, definition.as_ref());
                    }
                }
            }
        }

        definitions
            .into_iter()
            .filter_map(|(node, definition)| {
                let name = definition.name()?.to_string();

                let ty = if definition.downcast_ref::<VariableDefinition>().is_some() {
                    "variable"
                } else if definition.downcast_ref::<ConstantDefinition>().is_some() {
                    "function"
                } else if definition.downcast_ref::<TypeDefinition>().is_some()
                    || definition
                        .downcast_ref::<StructureConstructorDefinition>()
                        .is_some()
                {
                    "class"
                } else if definition.downcast_ref::<TraitDefinition>().is_some() {
                    "interface"
                } else if definition
                    .downcast_ref::<TypeParameterDefinition>()
                    .is_some()
                {
                    "typeParameter"
                } else if definition
                    .downcast_ref::<MarkerConstructorDefinition>()
                    .is_some()
                    || definition
                        .downcast_ref::<VariantConstructorDefinition>()
                        .is_some()
                {
                    "constructor"
                } else {
                    return None;
                };

                let mut ctx = RenderCtx::with_filter(&default_filter);
                ctx.node(node);

                let (rendered, _) =
                    ctx.finish(&self.result.db, |db, segment| segment.plain_text(db));

                Some(IdeDefinition {
                    name,
                    ty: ty.to_string(),
                    definition: rendered.to_string(),
                    comments: self.comments(node),
                })
            })
            .collect()
    }

    fn node_at(&self, line: usize, column: usize) -> Option<Node> {
        self.result
            .db
            .owned_nodes()
            .filter(|&node| !self.result.db.is_hidden(node))
            .filter_map(|node| {
                let span = self
                    .result
                    .db
                    .get::<Syntax>(node)?
                    .0
                    .get(&self.result.db)
                    .span(&self.result.db);

                if span.start.line == line
                    && span.start.column <= column
                    && span.end.line == line
                    && span.end.column >= column
                {
                    Some((node, span.end.column - span.start.column))
                } else {
                    None
                }
            })
            .min_by_key(|(_, length)| *length)
            .map(|(node, _)| node)
    }

    fn comments(&self, node: Node) -> Option<String> {
        let comments = wipple_queries::comments_without_links(&self.result.db, node)?;

        let mut writer = FeedbackWriter::with_filter(&default_filter);
        writer.comments(&self.result.db, node, &comments);

        let feedback = writer.finish(&self.result.db, |db, segment| segment.markdown(db, false));

        (!feedback.message.is_empty()).then_some(feedback.message)
    }
}

impl From<&Span> for IdeRange {
    fn from(span: &Span) -> Self {
        IdeRange {
            start: IdePosition {
                line: span.start.line,
                column: span.start.column,
            },
            end: IdePosition {
                line: span.end.line,
                column: span.end.column,
            },
        }
    }
}
