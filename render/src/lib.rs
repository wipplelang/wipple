//! Render compiler output as strings.

#![allow(missing_docs)] // TODO: Documentation

use futures::{
    future::{self, BoxFuture},
    FutureExt,
};
use line_index::LineIndex;
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    ops::ControlFlow,
    sync::{Arc, Mutex},
};

type WithInfo<T> = wipple_driver::util::WithInfo<wipple_driver::Info, T>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnyDeclaration {
    pub name: Option<String>,
    pub path: wipple_driver::lower::Path,
    pub kind: AnyDeclarationKind,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AnyDeclarationKind {
    Type(wipple_driver::typecheck::TypeDeclaration<wipple_driver::Driver>),
    Trait(wipple_driver::typecheck::TraitDeclaration<wipple_driver::Driver>),
    TypeParameter(wipple_driver::typecheck::TypeParameterDeclaration<wipple_driver::Driver>),
    Constant(wipple_driver::typecheck::ConstantDeclaration<wipple_driver::Driver>),
    Instance(wipple_driver::typecheck::InstanceDeclaration<wipple_driver::Driver>),
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceLocation {
    pub line: u32,
    pub column: u32,
    pub index: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RenderedSourceLocation {
    pub path: String,
    pub visible_path: String,
    pub start: SourceLocation,
    pub end: SourceLocation,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RenderedDiagnostic {
    pub location: RenderedSourceLocation,
    pub severity: RenderedDiagnosticSeverity,
    pub message: String,
    pub fix: Option<RenderedFix>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum RenderedDiagnosticSeverity {
    Warning,
    Error,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RenderedFix {
    pub message: String,
    pub before: Option<String>,
    pub replacement: Option<String>,
    pub after: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RenderedDocumentation {
    pub docs: String,
    pub example: Option<String>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct RenderedHighlight {
    pub category: Option<String>,
    pub icon: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RenderedSuggestion {
    pub kind: RenderedSuggestionKind,
    pub name: String,
    pub code: Option<String>,
    pub docs: Option<RenderedDocumentation>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RenderedSuggestionKind {
    Type,
    Trait,
    TypeParameter,
    Constant,
    Variable,
    Keyword,
    Operator,
}

const KEYWORDS: &[&str] = &[
    "do",
    "when",
    "type",
    "trait",
    "instance",
    "intrinsic",
    "infer",
    "default",
];

const OPERATORS: &[&str] = &["as", "to", "by", "is", "and", "or"];

#[derive(Clone)]
pub struct Render(Arc<Mutex<RenderInner>>);

#[derive(Default)]
pub struct RenderInner {
    pub interface: Option<wipple_driver::Interface>,
    pub libraries: Vec<wipple_driver::Library>,
    pub ide: Option<wipple_driver::Ide>,
    files: HashMap<String, (wipple_driver::File, LineIndex)>,
    declarations: Vec<WithInfo<AnyDeclaration>>,
}

impl Render {
    pub fn new() -> Self {
        Render(Default::default())
    }

    pub async fn get_interface(&self) -> Option<wipple_driver::Interface> {
        self.0.lock().unwrap().interface.clone()
    }

    pub async fn update(
        &self,
        interface: wipple_driver::Interface,
        libraries: Vec<wipple_driver::Library>,
        ide: Option<wipple_driver::Ide>,
    ) {
        let mut inner = self.0.lock().unwrap();

        inner.files = interface
            .files
            .iter()
            .map(|file| {
                (
                    file.path.clone(),
                    (file.clone(), LineIndex::new(&file.code)),
                )
            })
            .collect();

        inner.declarations = Vec::new();
        macro_rules! insert_declaration {
            ($decl:ident($ty:ident)) => {
                inner.declarations.extend(
                    interface
                        .$decl
                        .iter()
                        .map(|(path, declaration)| WithInfo {
                            info: declaration.info.clone(),
                            item: AnyDeclaration {
                                name: self.name_for_path(&path),
                                path: path.clone(),
                                kind: AnyDeclarationKind::$ty(declaration.item.clone()),
                            },
                        })
                        .collect::<Vec<_>>(),
                );
            };
            ($($decl:ident($ty:ident)),* $(,)?) => {
                $(
                    insert_declaration!($decl($ty));
                )*
            }
        }

        insert_declaration!(
            type_declarations(Type),
            trait_declarations(Trait),
            type_parameter_declarations(TypeParameter),
            constant_declarations(Constant),
            instance_declarations(Instance),
        );

        inner.interface = Some(interface);
        inner.libraries = libraries;
        inner.ide = ide;
    }

    pub async fn get_declaration_from_path(
        &self,
        path: &wipple_driver::lower::Path,
    ) -> Option<WithInfo<AnyDeclaration>> {
        self.0
            .lock()
            .unwrap()
            .declarations
            .iter()
            .find(|declaration| declaration.item.path == *path)
            .cloned()
    }

    pub async fn get_declaration_from_info(
        &self,
        info: &wipple_driver::Info,
        between: bool,
    ) -> Option<WithInfo<AnyDeclaration>> {
        self.0
            .lock()
            .unwrap()
            .declarations
            .iter()
            .find(|declaration| self.compare_info(&declaration.info, info, between))
            .cloned()
    }

    pub async fn render_source<T>(&self, value: &WithInfo<T>) -> Option<String> {
        let inner = self.0.lock().unwrap();
        let (file, _) = inner.files.get(value.info.location.path.as_ref())?;
        Some(file.code.clone())
    }

    pub async fn render_source_location<T>(
        &self,
        value: &WithInfo<T>,
    ) -> Option<RenderedSourceLocation> {
        let inner = self.0.lock().unwrap();
        let (file, line_index) = inner.files.get(value.info.location.path.as_ref())?;

        let start_location =
            line_index.try_line_col(line_index::TextSize::new(value.info.location.span.start))?;

        let end_location =
            line_index.try_line_col(line_index::TextSize::new(value.info.location.span.end))?;

        Some(RenderedSourceLocation {
            path: file.path.clone(),
            visible_path: file.visible_path.clone(),
            start: SourceLocation {
                line: start_location.line,
                column: start_location.col,
                index: value.info.location.span.start,
            },
            end: SourceLocation {
                line: end_location.line,
                column: end_location.col,
                index: value.info.location.span.end,
            },
        })
    }

    pub async fn render_code<T>(&self, value: &WithInfo<T>) -> Option<String> {
        let rendered_source_location = self.render_source_location(value).await?;

        let inner = self.0.lock().unwrap();
        let (file, _) = inner.files.get(&rendered_source_location.path)?;

        Some(
            file.code[rendered_source_location.start.index as usize
                ..rendered_source_location.end.index as usize]
                .to_string(),
        )
    }

    pub async fn render_declaration(
        &self,
        declaration: &WithInfo<AnyDeclaration>,
    ) -> Option<String> {
        match &declaration.item.kind {
            AnyDeclarationKind::Type(type_declaration) => {
                let type_function = self
                    .render_type_function(&type_declaration.parameters, &[])
                    .await;

                Some(format!(
                    "{} : {}type",
                    declaration.item.name.as_deref().unwrap_or("<unknown>"),
                    type_function,
                ))
            }
            AnyDeclarationKind::Trait(trait_declaration) => {
                let type_function = self
                    .render_type_function(&trait_declaration.parameters, &[])
                    .await;

                Some(format!(
                    "{} : {}trait",
                    declaration.item.name.as_deref().unwrap_or("<unknown>"),
                    type_function,
                ))
            }
            AnyDeclarationKind::TypeParameter(_) => declaration.item.name.clone(),
            AnyDeclarationKind::Constant(constant_declaration) => {
                let name = declaration.item.name.as_deref().unwrap_or("<unknown>");

                let r#type = self
                    .render_type(&constant_declaration.r#type, true, false, false)
                    .await;

                let type_function = self
                    .render_type_function(
                        &constant_declaration.parameters,
                        &constant_declaration.bounds,
                    )
                    .await;

                Some(format!("{} :: {}{}", name, type_function, r#type))
            }
            AnyDeclarationKind::Instance(instance_declaration) => {
                let type_function = self
                    .render_type_function(
                        &instance_declaration.parameters,
                        &instance_declaration.bounds,
                    )
                    .await;

                let instance = self.render_instance(&instance_declaration.instance).await;

                Some(format!("{}instance {}", type_function, instance))
            }
        }
    }

    pub fn render_pattern<'a>(
        &'a self,
        pattern: &'a wipple_driver::typecheck::exhaustiveness::Pattern<wipple_driver::Driver>,
        is_top_level: bool,
    ) -> BoxFuture<'a, String> {
        async move {
            match pattern {
                wipple_driver::typecheck::exhaustiveness::Pattern::Constructor(
                    constructor,
                    values,
                ) => match constructor {
                    wipple_driver::typecheck::exhaustiveness::Constructor::Variant(path) => {
                        let declaration = match self.get_declaration_from_path(path).await {
                            Some(declarataion) => declarataion,
                            None => return String::from("<unknown>"),
                        };

                        let name = declaration.item.name.as_deref().unwrap_or("<unknown>");

                        let rendered = if values.is_empty() {
                            name.to_string()
                        } else {
                            let values = future::join_all(
                                values
                                    .iter()
                                    .map(|pattern| self.render_pattern(pattern, false)),
                            )
                            .await
                            .join(" ");

                            format!("{} {}", name, values)
                        };

                        if is_top_level || values.is_empty() {
                            rendered
                        } else {
                            format!("({})", rendered)
                        }
                    }
                    wipple_driver::typecheck::exhaustiveness::Constructor::Tuple => {
                        let rendered = if values.is_empty() {
                            String::from("()")
                        } else if values.len() == 1 {
                            format!(
                                "{} ;",
                                self.render_pattern(values.first().unwrap(), false).await,
                            )
                        } else {
                            future::join_all(
                                values
                                    .iter()
                                    .map(|pattern| self.render_pattern(pattern, false)),
                            )
                            .await
                            .join(" ; ")
                        };

                        if is_top_level || values.is_empty() {
                            rendered
                        } else {
                            format!("({})", rendered)
                        }
                    }
                    wipple_driver::typecheck::exhaustiveness::Constructor::Structure => {
                        String::from("{...}")
                    }
                    wipple_driver::typecheck::exhaustiveness::Constructor::Wrapper(path) => {
                        let declaration = match self.get_declaration_from_path(path).await {
                            Some(declarataion) => declarataion,
                            None => return String::from("<unknown>"),
                        };

                        let name = declaration.item.name.as_deref().unwrap_or("<unknown>");

                        let value = self.render_pattern(values.first().unwrap(), false).await;

                        if is_top_level {
                            format!("{} {}", name, value)
                        } else {
                            format!("({} {})", name, value)
                        }
                    }
                    wipple_driver::typecheck::exhaustiveness::Constructor::Unbounded => {
                        String::from("_")
                    }
                },
                wipple_driver::typecheck::exhaustiveness::Pattern::Binding => String::from("_"),
                wipple_driver::typecheck::exhaustiveness::Pattern::Or(patterns) => {
                    let rendered = future::join_all(
                        patterns
                            .iter()
                            .map(|pattern| self.render_pattern(pattern, false)),
                    )
                    .await
                    .join(" or ");

                    if is_top_level {
                        rendered
                    } else {
                        format!("({})", rendered)
                    }
                }
            }
        }
        .boxed()
    }

    pub fn render_type<'a>(
        &'a self,
        r#type: &'a WithInfo<wipple_driver::typecheck::Type<wipple_driver::Driver>>,
        is_top_level: bool,
        describe: bool,
        render_as_code: bool,
    ) -> BoxFuture<'a, String> {
        async move {
            if is_top_level && describe {
                let message = async {
                    let result = wipple_driver::resolve_attribute_like_trait(
                        "describe-type",
                        r#type.clone(),
                        1,
                        self.get_interface().await?,
                    )?;

                    match result.into_iter().next()?.item {
                        wipple_driver::typecheck::Type::Message { segments, trailing } => {
                            Some(wipple_driver::typecheck::CustomMessage { segments, trailing })
                        }
                        _ => None,
                    }
                }
                .await;

                if let Some(message) = message {
                    return self.render_custom_message(&message, false).await;
                }
            }

            fn render_type_inner(
                render: &Render,
                r#type: WithInfo<&wipple_driver::typecheck::Type<wipple_driver::Driver>>,
                is_top_level: bool,
                is_return: bool,
            ) -> String {
                match &r#type.item {
                    wipple_driver::typecheck::Type::Unknown => String::from("_"),
                    wipple_driver::typecheck::Type::Parameter(parameter) => render
                        .name_for_path(parameter)
                        .unwrap_or_else(|| String::from("<unknown>")),
                    wipple_driver::typecheck::Type::Declared { path, parameters }
                    | wipple_driver::typecheck::Type::Alias { path, parameters } => {
                        let name = render
                            .name_for_path(path)
                            .unwrap_or_else(|| String::from("<unknown>"));

                        let rendered = if parameters.is_empty() {
                            name
                        } else {
                            format!(
                                "{} {}",
                                name,
                                parameters
                                    .iter()
                                    .map(|parameter| render_type_inner(
                                        render,
                                        parameter.as_ref(),
                                        false,
                                        false
                                    ))
                                    .collect::<Vec<_>>()
                                    .join(" "),
                            )
                        };

                        if is_top_level || is_return || parameters.is_empty() {
                            rendered
                        } else {
                            format!("({})", rendered)
                        }
                    }
                    wipple_driver::typecheck::Type::Function { inputs, output } => {
                        let inputs = inputs
                            .iter()
                            .map(|input| render_type_inner(render, input.as_ref(), false, false))
                            .collect::<Vec<_>>()
                            .join(" ");

                        let output = render_type_inner(render, output.as_deref(), false, true);

                        let rendered = format!("{} -> {}", inputs, output);

                        if is_top_level || is_return {
                            rendered
                        } else {
                            format!("({})", rendered)
                        }
                    }
                    wipple_driver::typecheck::Type::Tuple(elements) => {
                        let rendered = if elements.is_empty() {
                            String::from("Unit")
                        } else if elements.len() == 1 {
                            format!(
                                "{} ;",
                                render_type_inner(
                                    render,
                                    elements.first().unwrap().as_ref(),
                                    false,
                                    false,
                                ),
                            )
                        } else {
                            elements
                                .iter()
                                .map(|r#type| {
                                    render_type_inner(render, r#type.as_ref(), false, false)
                                })
                                .collect::<Vec<_>>()
                                .join(" ; ")
                        };

                        if is_top_level || is_return || elements.is_empty() {
                            rendered
                        } else {
                            format!("({})", rendered)
                        }
                    }
                    wipple_driver::typecheck::Type::Block(value) => {
                        format!(
                            "{{{}}}",
                            render_type_inner(render, value.as_deref(), true, false)
                        )
                    }
                    wipple_driver::typecheck::Type::Intrinsic => String::from("intrinsic"),
                    wipple_driver::typecheck::Type::Message { segments, trailing } => {
                        let mut message = String::new();

                        for segment in segments {
                            message.push_str(&segment.text);

                            message.push_str(&render_type_inner(
                                render,
                                segment.r#type.as_ref(),
                                true,
                                true,
                            ));
                        }

                        message.push_str(trailing);

                        message
                    }
                    wipple_driver::typecheck::Type::Equal { left, right } => {
                        let rendered = format!(
                            "{} = {}",
                            render_type_inner(render, left.as_deref(), false, false),
                            render_type_inner(render, right.as_deref(), false, false),
                        );

                        if is_top_level || is_return {
                            rendered
                        } else {
                            format!("({})", rendered)
                        }
                    }
                }
            }

            let rendered = render_type_inner(self, r#type.as_ref(), is_top_level, true);

            if render_as_code {
                format!("`{}`", rendered)
            } else {
                rendered
            }
        }
        .boxed()
    }

    pub async fn render_type_function(
        &self,
        parameters: &[wipple_driver::lower::Path],
        bounds: &[WithInfo<wipple_driver::typecheck::Instance<wipple_driver::Driver>>],
    ) -> String {
        if parameters.is_empty() {
            return String::new();
        }

        let rendered_parameters = parameters
            .iter()
            .filter_map(|parameter| self.name_for_path(parameter))
            .collect::<Vec<_>>()
            .join(" ");

        let rendered_bounds =
            future::join_all(bounds.iter().map(|bound| self.render_instance(bound)))
                .await
                .join(" ");

        if rendered_bounds.is_empty() {
            format!("{} => ", rendered_parameters)
        } else {
            format!("{} where {} => ", rendered_parameters, rendered_bounds)
        }
    }

    pub async fn render_instance(
        &self,
        instance: &WithInfo<wipple_driver::typecheck::Instance<wipple_driver::Driver>>,
    ) -> String {
        let r#trait = self
            .name_for_path(&instance.item.r#trait)
            .unwrap_or_else(|| String::from("<unknown>"));

        let parameters = future::join_all(
            instance
                .item
                .parameters
                .iter()
                .map(|r#type| self.render_type(r#type, false, false, false)),
        )
        .await;

        if parameters.is_empty() {
            r#trait
        } else {
            format!("({} {})", r#trait, parameters.join(" "))
        }
    }

    pub async fn render_custom_message(
        &self,
        message: &wipple_driver::typecheck::CustomMessage<wipple_driver::Driver>,
        render_as_code: bool,
    ) -> String {
        let render_segments_as_code = message.segments.first().map_or_else(
            || message.trailing.starts_with('`'),
            |segment| segment.text.starts_with('`'),
        ) && message.trailing.ends_with('`');

        let mut result = String::new();
        for segment in &message.segments {
            let code = if render_segments_as_code || segment.text.ends_with('`') {
                self.render_code(&segment.r#type).await
            } else {
                None
            };

            result.push_str(&segment.text);

            result.push_str(&match code {
                Some(code) => code,
                None => self.render_type(&segment.r#type, true, true, true).await,
            });
        }

        result.push_str(&message.trailing);

        if render_segments_as_code && !render_as_code {
            result = result[1..result.len() - 1].to_string();
        }

        result
    }

    pub async fn render_diagnostic(
        &self,
        diagnostic: &WithInfo<wipple_driver::Diagnostic>,
    ) -> Option<RenderedDiagnostic> {
        let rendered_source_location = self
            .render_source_location(diagnostic)
            .await
            .unwrap_or_else(|| RenderedSourceLocation {
                path: diagnostic.info.location.path.to_string(),
                visible_path: diagnostic.info.location.visible_path.to_string(),
                start: Default::default(),
                end: Default::default(),
            });

        let severity;
        let message;
        let mut fix = None;
        match &diagnostic.item {
            wipple_driver::Diagnostic::Tokenize(tokenize_diagnostic) => match tokenize_diagnostic {
                wipple_driver::syntax::tokenize::Diagnostic::InvalidToken => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = String::from("unrecognized symbol");
                    fix = Some(RenderedFix {
                        message: String::from("remove the invalid symbol"),
                        before: None,
                        replacement: Some(String::new()),
                        after: None,
                    });
                }
                wipple_driver::syntax::tokenize::Diagnostic::Mismatch {
                    expected, found, ..
                } => {
                    severity = RenderedDiagnosticSeverity::Error;

                    match (expected, found) {
                        (Some(expected), Some(found)) => {
                            message = format!(
                                "expected {} here, but found {}",
                                self.render_token(expected, "a").await,
                                self.render_token(found, "a").await,
                            );
                        }
                        (Some(expected), None) => {
                            message = format!(
                                "expected {} here",
                                self.render_token(expected, "a").await,
                            );
                        }
                        (None, Some(found)) => {
                            message =
                                format!("unexpected {} here", self.render_token(found, "a").await);

                            fix = Some(RenderedFix {
                                message: format!(
                                    "remove {}",
                                    self.render_token(found, "this").await
                                ),
                                before: None,
                                replacement: Some(String::new()),
                                after: None,
                            });
                        }
                        (None, None) => return None,
                    }
                }
            },
            wipple_driver::Diagnostic::Parse(parse_diagnostic) => {
                match parse_diagnostic.direction {
                    Some(wipple_driver::syntax::parse::Direction::Before) => {
                        severity = RenderedDiagnosticSeverity::Error;
                        message = format!(
                            "expected {} before this",
                            self.render_syntax_kind(parse_diagnostic.expected).await,
                        );
                    }
                    Some(wipple_driver::syntax::parse::Direction::After) => {
                        severity = RenderedDiagnosticSeverity::Error;
                        message = format!(
                            "expected {} after this",
                            self.render_syntax_kind(parse_diagnostic.expected).await,
                        );
                    }
                    None => {
                        severity = RenderedDiagnosticSeverity::Error;
                        message = format!(
                            "expected {} here",
                            self.render_syntax_kind(parse_diagnostic.expected).await,
                        );
                    }
                }
            }
            wipple_driver::Diagnostic::Syntax(syntax_diagnostic) => match syntax_diagnostic {
                wipple_driver::syntax::Diagnostic::UnexpectedBound => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = String::from("bounds aren't allowed on type and trait definitions");
                }
                wipple_driver::syntax::Diagnostic::ExpectedConstantValue(value) => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = format!("missing a value for `{}` on the next line", value,);
                }
                wipple_driver::syntax::Diagnostic::EmptyTypeRepresentation => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message =
                        String::from("missing a field or variant between the braces in this type");
                }
                wipple_driver::syntax::Diagnostic::ExpectedField => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = String::from("expected a field of the form `name :: Type` here");
                }
                wipple_driver::syntax::Diagnostic::ExpectedVariant => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = String::from("expected a variant of the form `Name` here");
                }
                wipple_driver::syntax::Diagnostic::InvalidTextLiteral(error) => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = error.error.clone();
                }
                wipple_driver::syntax::Diagnostic::InvalidPlaceholderText { expected, found } => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = format!(
                        "text has {} placeholders, but {} inputs were provided here",
                        expected, found,
                    );
                }
            },
            wipple_driver::Diagnostic::Lower(lower_diagnostic) => match lower_diagnostic {
                wipple_driver::lower::Diagnostic::UnresolvedName(name) => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = format!("can't find `{}`", name);
                }
                wipple_driver::lower::Diagnostic::UnresolvedType(name) => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = format!("can't find type `{}`", name);
                }
                wipple_driver::lower::Diagnostic::UnresolvedTrait(name) => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = format!("can't find trait `{}`", name);
                }
                wipple_driver::lower::Diagnostic::UnresolvedVariant(name) => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = format!("can't find variant `{}`", name);
                }
                wipple_driver::lower::Diagnostic::UnresolvedLanguageItem(name) => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = format!("can't find language item `{}`", name);
                }
                wipple_driver::lower::Diagnostic::AmbiguousName { name, .. } => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = format!("`{}` has multiple definitions", name);
                }
                wipple_driver::lower::Diagnostic::AlreadyDefined(name) => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = format!("`{}` is already defined", name);
                }
                wipple_driver::lower::Diagnostic::NestedLanguageDeclaration => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = String::from("language items must be declared at the top level");
                }
                wipple_driver::lower::Diagnostic::NotAWrapper => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = String::from(
                        "this pattern matches a structure or enumeration, not a wrapper type",
                    );
                }
                wipple_driver::lower::Diagnostic::WrapperExpectsASinglePattern => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = String::from("expected a single pattern after the name of the type");
                }
                wipple_driver::lower::Diagnostic::InvalidMutatePattern => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = String::from("`!` only works when assigning to a variable using `:`");
                }
                wipple_driver::lower::Diagnostic::MissingTypes(count) => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = format!("missing {} types here", count);
                }
                wipple_driver::lower::Diagnostic::ExtraType => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = String::from("extra type provided here");
                }
            },
            wipple_driver::Diagnostic::Typecheck(typecheck_diagnostic) => {
                match typecheck_diagnostic {
                    wipple_driver::typecheck::Diagnostic::RecursionLimit => {
                        severity = RenderedDiagnosticSeverity::Error;
                        message =
                            String::from("this code is too complex to check; try simplifying it");
                    }
                    wipple_driver::typecheck::Diagnostic::MissingLanguageItem(name) => {
                        severity = RenderedDiagnosticSeverity::Error;
                        message =
                            format!("checking this code requires the `{}` language item", name);
                    }
                    wipple_driver::typecheck::Diagnostic::UnknownType(r#type) => {
                        severity = RenderedDiagnosticSeverity::Error;

                        let rendered_type = self
                            .render_type(
                                &WithInfo {
                                    info: diagnostic.info.clone(),
                                    item: r#type.clone(),
                                },
                                true,
                                false,
                                true,
                            )
                            .await;

                        if rendered_type == "`_`" {
                            message = String::from(
                                "could not determine what kind of value this code produces",
                            );
                        } else {
                            message = format!(
                                "this code produces {}, but the `_`s are unknown",
                                rendered_type,
                            );
                        }
                    }
                    wipple_driver::typecheck::Diagnostic::UndeclaredTypeParameter(name) => {
                        severity = RenderedDiagnosticSeverity::Error;
                        message = format!(
                        "this code references the type parameter `{}`, which isn't available here",
                        name,
                    );
                    }
                    wipple_driver::typecheck::Diagnostic::Mismatch {
                        actual, expected, ..
                    } => {
                        severity = RenderedDiagnosticSeverity::Error;

                        let expected_message = self.render_type(expected, true, true, true).await;

                        let actual_message = self.render_type(actual, true, true, true).await;

                        // If the type descriptions are equal, try rendering the actual type by setting
                        // `describe` to false
                        let (expected_message, actual_message) =
                            if expected_message == actual_message {
                                (
                                    self.render_type(expected, true, false, true).await,
                                    self.render_type(actual, true, false, true).await,
                                )
                            } else {
                                (expected_message, actual_message)
                            };

                        message = format!(
                            "expected {} here, but found {}",
                            expected_message, actual_message,
                        );
                    }
                    wipple_driver::typecheck::Diagnostic::MissingInputs(inputs) => {
                        let code = self.render_code(diagnostic).await.unwrap_or_default();

                        let inputs = future::join_all(
                            inputs
                                .iter()
                                .map(|r#type| self.render_type(r#type, true, true, true)),
                        )
                        .await;

                        severity = RenderedDiagnosticSeverity::Error;

                        message = match inputs.len() {
                            1 => format!("missing {} for `{}`", inputs[0], code),
                            2 => format!("missing {} and {} for `{}`", inputs[0], inputs[1], code),
                            _ => format!(
                                "missing {}, and {} for `{}`",
                                inputs[..inputs.len() - 1].join(", "),
                                inputs[inputs.len() - 1],
                                code,
                            ),
                        };
                    }
                    wipple_driver::typecheck::Diagnostic::ExtraInput => {
                        let code = self.render_code(diagnostic).await.unwrap_or_default();

                        severity = RenderedDiagnosticSeverity::Error;
                        message = format!("extra input provided to `{}`", code);
                    }
                    wipple_driver::typecheck::Diagnostic::UnresolvedInstance {
                        instance, ..
                    } => {
                        let code = self.render_code(diagnostic).await.unwrap_or_default();

                        let rendered_instance = self
                            .render_instance(&WithInfo {
                                info: diagnostic.info.clone(),
                                item: instance.clone(),
                            })
                            .await;

                        severity = RenderedDiagnosticSeverity::Error;
                        message = format!("`{}` requires `{}`", code, rendered_instance);
                    }
                    wipple_driver::typecheck::Diagnostic::TraitHasNoValue(_) => {
                        let code = self.render_code(diagnostic).await.unwrap_or_default();

                        severity = RenderedDiagnosticSeverity::Error;
                        message = format!("`{}` can't be used as a value", code);
                    }
                    wipple_driver::typecheck::Diagnostic::ExpectedInstanceValue => {
                        severity = RenderedDiagnosticSeverity::Error;
                        message = String::from("`instance` declaration is missing a value");
                    }
                    wipple_driver::typecheck::Diagnostic::UnexpectedInstanceValue => {
                        severity = RenderedDiagnosticSeverity::Error;
                        message = String::from(
                            "`instance` declaration declares a value, but the trait doesn't",
                        );
                    }
                    wipple_driver::typecheck::Diagnostic::NotAStructure(r#type) => {
                        let rendered_type = self.render_type(r#type, true, true, true).await;

                        severity = RenderedDiagnosticSeverity::Error;
                        message = format!("{} is not a structure", rendered_type);
                    }
                    wipple_driver::typecheck::Diagnostic::MissingFields(r#type) => {
                        let rendered_fields = r#type
                            .iter()
                            .map(|field| format!("`{}`", field))
                            .collect::<Vec<_>>()
                            .join(", ");

                        severity = RenderedDiagnosticSeverity::Error;
                        message = format!("missing fields {}", rendered_fields);
                    }
                    wipple_driver::typecheck::Diagnostic::ExtraField => {
                        severity = RenderedDiagnosticSeverity::Error;
                        message = String::from("extra field");
                    }
                    wipple_driver::typecheck::Diagnostic::OverlappingInstances { .. } => {
                        severity = RenderedDiagnosticSeverity::Error;
                        message = String::from(
                            "this instance already exists elsewhere; try making it more specific",
                        );
                    }
                    wipple_driver::typecheck::Diagnostic::MissingPatterns(patterns) => {
                        let last = patterns.last().unwrap();

                        severity = RenderedDiagnosticSeverity::Error;
                        message = if patterns.len() == 1 {
                            if matches!(
                                last,
                                wipple_driver::typecheck::exhaustiveness::Pattern::Binding
                            ) {
                                String::from("missing variable to handle remaining patterns")
                            } else {
                                format!(
                                    "this code doesn't handle {}",
                                    self.render_pattern(last, true).await,
                                )
                            }
                        } else {
                            format!(
                                "this code doesn't handle {} or {}",
                                future::join_all(
                                    patterns[..patterns.len() - 1]
                                        .iter()
                                        .map(|pattern| self.render_pattern(pattern, true)),
                                )
                                .await
                                .join(", "),
                                self.render_pattern(last, true).await,
                            )
                        };
                    }
                    wipple_driver::typecheck::Diagnostic::ExtraPattern => {
                        severity = RenderedDiagnosticSeverity::Warning;
                        message = String::from(
                            "this pattern is unnecessary because it is already handled above",
                        );
                        fix = Some(RenderedFix {
                            message: String::from("remove this pattern"),
                            before: None,
                            replacement: Some(String::new()),
                            after: None,
                        });
                    }
                    wipple_driver::typecheck::Diagnostic::Custom {
                        message: custom_message,
                        fix: custom_fix,
                    } => {
                        severity = RenderedDiagnosticSeverity::Error;
                        message = self.render_custom_message(custom_message, true).await;

                        if let Some(custom_fix) = custom_fix {
                            fix = Some(RenderedFix {
                                message: self.render_custom_message(&custom_fix.0, true).await,
                                before: None,
                                replacement: Some(
                                    self.render_custom_message(&custom_fix.1, false).await,
                                ),
                                after: None,
                            });
                        }
                    }
                }
            }
            wipple_driver::Diagnostic::Ir => {
                severity = RenderedDiagnosticSeverity::Error;
                message = String::from("failed to produce IR for this code");
            }
        }

        Some(RenderedDiagnostic {
            location: rendered_source_location,
            severity,
            message,
            fix,
        })
    }

    pub async fn render_token(
        &self,
        token: &wipple_driver::syntax::tokenize::Token<'_>,
        prefix: &str,
    ) -> String {
        use wipple_driver::syntax::tokenize::Token;

        let alt_prefix = if prefix == "a" { "the" } else { "this" };

        match token {
            Token::Number(_) => format!("{} number", prefix),
            Token::LeftParenthesis => {
                format!("{} opening parenthesis (`(`)", prefix)
            }
            Token::RightParenthesis => {
                format!("{} closing parenthesis (`)`)", prefix)
            }
            Token::LeftBracket => {
                format!("{} opening bracket (`[`)", prefix)
            }
            Token::RightBracket => {
                format!("{} closing bracket (`]`)", prefix)
            }
            Token::LeftBrace => {
                format!("{} opening brace (`{{`)", prefix)
            }
            Token::RightBrace => {
                format!("{} closing brace (`}}`)", prefix)
            }
            Token::LineBreak => {
                format!("{} end of the line", alt_prefix)
            }
            Token::Comment(_) => format!("{} comment", prefix),
            Token::Keyword(value) => {
                format!("{} word `{}`", alt_prefix, value)
            }
            Token::Operator(value) => {
                format!("{} symbol `{}`", alt_prefix, value)
            }
            Token::VariadicOperator(value) => {
                format!("{} symbol `{}`", alt_prefix, value)
            }
            Token::NonAssociativeOperator(value) => {
                format!("{} symbol `{}`", alt_prefix, value)
            }
            Token::Name(_) => format!("{} name", prefix),
            Token::Text(_) => format!("{} piece of text", prefix),
        }
    }

    pub async fn render_syntax_kind(
        &self,
        kind: wipple_driver::syntax::parse::SyntaxKind,
    ) -> &'static str {
        use wipple_driver::syntax::parse::SyntaxKind;

        match kind {
            SyntaxKind::Number => "number",
            SyntaxKind::Name => "name",
            SyntaxKind::Text => "text",
            SyntaxKind::TopLevel => "top level",
            SyntaxKind::Attribute => "attribute",
            SyntaxKind::AttributeValue => "attribute value",
            SyntaxKind::Statement => "statement",
            SyntaxKind::Keyword => "keyword",
            SyntaxKind::Operator => "operator",
            SyntaxKind::Instance => "instance",
            SyntaxKind::TypeParameter => "type parameter",
            SyntaxKind::Pattern => "pattern",
            SyntaxKind::WildcardPattern => "wildcard pattern",
            SyntaxKind::NumberPattern => "number pattern",
            SyntaxKind::TextPattern => "text pattern",
            SyntaxKind::VariantPattern => "variant pattern",
            SyntaxKind::DestructurePattern => "destructure pattern",
            SyntaxKind::TuplePattern => "tuple pattern",
            SyntaxKind::OrPattern => "or pattern",
            SyntaxKind::MutatePattern => "mutate pattern",
            SyntaxKind::AnnotatePattern => "annotate pattern",
            SyntaxKind::Expression => "expression",
            SyntaxKind::Type => "type",
            SyntaxKind::PlaceholderType => "placeholder type",
            SyntaxKind::DeclaredType => "declared type",
            SyntaxKind::FunctionType => "function type",
            SyntaxKind::TupleType => "tuple type",
            SyntaxKind::BlockType => "block type",
            SyntaxKind::IntrinsicType => "intrinsic type",
            SyntaxKind::MessageType => "message type",
            SyntaxKind::EqualType => "equal type",
            SyntaxKind::TypeMember => "type member",
            SyntaxKind::FieldDeclaration => "field declaration",
            SyntaxKind::VariantDeclaration => "variant declaration",
            SyntaxKind::Arm => "arm",
            SyntaxKind::TypeFunction => "type function",
            SyntaxKind::TypeRepresentation => "type representation",
            SyntaxKind::TypeDeclaration => "type declaration",
            SyntaxKind::TypeAliasDeclaration => "type alias declaration",
            SyntaxKind::TraitDeclaration => "trait declaration",
            SyntaxKind::InstanceDeclaration => "instance declaration",
            SyntaxKind::ConstantDeclaration => "constant declaration",
            SyntaxKind::LanguageDeclaration => "language declaration",
            SyntaxKind::Assignment => "assignment",
            SyntaxKind::AnnotateExpression => "annotate expression",
            SyntaxKind::NameExpression => "name expression",
            SyntaxKind::NumberExpression => "number expression",
            SyntaxKind::TextExpression => "text expression",
            SyntaxKind::DoExpression => "do expression",
            SyntaxKind::CallExpression => "call expression",
            SyntaxKind::ApplyExpression => "apply expression",
            SyntaxKind::BinaryOperatorExpression => "binary operator expression",
            SyntaxKind::AsExpression => "as expression",
            SyntaxKind::IsExpression => "is expression",
            SyntaxKind::WhenExpression => "when expression",
            SyntaxKind::IntrinsicExpression => "intrinsic expression",
            SyntaxKind::TupleExpression => "tuple expression",
            SyntaxKind::CollectionExpression => "collection expression",
            SyntaxKind::StructureExpression => "structure expression",
            SyntaxKind::StructureField => "structure field",
            SyntaxKind::WhenBody => "when body",
            SyntaxKind::WhenArm => "when arm",
            SyntaxKind::BlockExpression => "block expression",
            SyntaxKind::FunctionExpression => "function expression",
            SyntaxKind::FunctionInputs => "function inputs",
            SyntaxKind::Nothing => "nothing",
        }
    }

    pub async fn render_diagnostic_to_debug_string(
        &self,
        diagnostic: &RenderedDiagnostic,
    ) -> String {
        let line = diagnostic.location.start.line + 1;
        let column = diagnostic.location.start.column + 1;

        let severity = match diagnostic.severity {
            RenderedDiagnosticSeverity::Warning => "warning",
            RenderedDiagnosticSeverity::Error => "error",
        };

        format!(
            "{}:{}:{}: {}: {}",
            diagnostic.location.visible_path, line, column, severity, diagnostic.message
        )
    }

    pub async fn render_documentation(
        &self,
        declaration: &WithInfo<AnyDeclaration>,
    ) -> Option<RenderedDocumentation> {
        let rendered_source_location = self.render_source_location(declaration).await?;

        let line = rendered_source_location.start.line as usize;
        if line == 0 {
            return None;
        }

        let inner = self.0.lock().unwrap();
        let lines = inner
            .files
            .get(&rendered_source_location.path)?
            .0
            .code
            .split('\n')
            .take(line)
            .collect::<Vec<_>>();

        let mut doc_lines = Vec::new();
        for code in lines.into_iter().rev() {
            let code = code.trim_start();

            if let Some(doc) = code.strip_prefix("--") {
                doc_lines.push(doc);
            } else if code.starts_with('@') {
                // Allow attributes between the comments and the declaration
                continue;
            } else {
                break;
            }
        }

        if doc_lines.is_empty() {
            return None;
        }

        doc_lines.reverse();

        let docs = doc_lines.join("\n");

        let attributes = match &declaration.item.kind {
            AnyDeclarationKind::Type(declaration) => Some(&declaration.attributes),
            AnyDeclarationKind::Trait(declaration) => Some(&declaration.attributes),
            AnyDeclarationKind::Constant(declaration) => Some(&declaration.attributes),
            AnyDeclarationKind::TypeParameter(_) | AnyDeclarationKind::Instance(_) => None,
        };

        let example = attributes.and_then(|attributes| {
            attributes.iter().find_map(|attribute| {
                if let wipple_driver::typecheck::Attribute::Valued { name, value } = &attribute.item
                {
                    if name.item == "example" {
                        if let wipple_driver::typecheck::AttributeValue::Text(text) = &value.item {
                            return Some(text.item.clone());
                        }
                    }
                }

                None
            })
        });

        Some(RenderedDocumentation { docs, example })
    }

    pub async fn render_highlight<T>(&self, value: &WithInfo<T>) -> Option<RenderedHighlight> {
        let declaration = self.get_declaration_from_info(&value.info, false).await?;

        let attributes = match &declaration.item.kind {
            AnyDeclarationKind::Type(declaration) => Some(&declaration.attributes),
            AnyDeclarationKind::Trait(declaration) => Some(&declaration.attributes),
            AnyDeclarationKind::Constant(declaration) => Some(&declaration.attributes),
            AnyDeclarationKind::TypeParameter(_) | AnyDeclarationKind::Instance(_) => None,
        };

        let mut options = RenderedHighlight::default();

        if let Some(attributes) = attributes {
            for attribute in attributes {
                if let wipple_driver::typecheck::Attribute::Valued { name, value } = &attribute.item
                {
                    if let wipple_driver::typecheck::AttributeValue::Text(text) = &value.item {
                        if name.item == "highlight-category" {
                            options.category = Some(text.item.clone());
                        } else if name.item == "highlight-icon" {
                            options.icon = Some(text.item.clone());
                        }
                    }
                }
            }
        }

        if options.category.is_none() && options.icon.is_none() {
            return None;
        }

        Some(options)
    }

    pub async fn render_suggestions_at_cursor(
        &self,
        path: &str,
        index: u32,
    ) -> Vec<RenderedSuggestion> {
        let keyword_suggestions = KEYWORDS.iter().map(|keyword| RenderedSuggestion {
            kind: RenderedSuggestionKind::Keyword,
            name: keyword.to_string(),
            code: None,
            docs: None, // TODO: Documentation for keywords
        });

        let operator_suggestions = OPERATORS.iter().map(|operator| RenderedSuggestion {
            kind: RenderedSuggestionKind::Operator,
            name: operator.to_string(),
            code: None,
            docs: None, // TODO: Documentation for operators
        });

        let declarations = self.0.lock().unwrap().declarations.clone();
        let declaration_suggestions =
            future::join_all(declarations.into_iter().map(|declaration| async move {
                let kind = match &declaration.item.kind {
                    AnyDeclarationKind::Type(_) => RenderedSuggestionKind::Type,
                    AnyDeclarationKind::Trait(_) => RenderedSuggestionKind::Trait,
                    AnyDeclarationKind::Constant(_) => RenderedSuggestionKind::Constant,
                    AnyDeclarationKind::TypeParameter(_) => RenderedSuggestionKind::TypeParameter,
                    AnyDeclarationKind::Instance(_) => return None,
                };

                Some(RenderedSuggestion {
                    kind,
                    name: declaration.item.name.as_ref()?.to_string(),
                    code: self.render_declaration(&declaration).await,
                    docs: self.render_documentation(&declaration).await,
                })
            }))
            .await
            .into_iter()
            .flatten();

        let local_suggestions = future::join_all(
            self.get_locals_at_cursor(path, index)
                .await
                .into_iter()
                .map(|local| async {
                    let (kind, name, r#type) = local.item;

                    let code = match r#type {
                        Some(r#type) => {
                            let r#type = self.render_type(&r#type, true, false, false).await;
                            Some(format!("{} :: {}", name, r#type))
                        }
                        None => None,
                    };

                    RenderedSuggestion {
                        kind,
                        name,
                        code,
                        docs: None, // locals cannot have documentation
                    }
                }),
        )
        .await;

        keyword_suggestions
            .chain(operator_suggestions)
            .chain(declaration_suggestions)
            .chain(local_suggestions)
            .collect()
    }

    async fn get_locals_at_cursor(
        &self,
        path: &str,
        index: u32,
    ) -> Vec<
        WithInfo<(
            RenderedSuggestionKind,
            String,
            Option<WithInfo<wipple_driver::typecheck::Type<wipple_driver::Driver>>>,
        )>,
    > {
        let expression_tree = match self.get_expression_tree_at_cursor(path, index).await {
            Some(tree) => tree,
            None => return Vec::new(),
        };

        let mut locals = Vec::new();

        if let Some(declaration) = self
            .get_declaration_from_info(&expression_tree.first().unwrap().info, true)
            .await
        {
            let parameters = match &declaration.item.kind {
                AnyDeclarationKind::Constant(declaration) => Some(&declaration.parameters),
                AnyDeclarationKind::Instance(declaration) => Some(&declaration.parameters),
                _ => None,
            };

            if let Some(parameters) = parameters {
                for parameter in parameters {
                    if let Some(declaration) = self.get_declaration_from_path(parameter).await {
                        if let Some(name) = declaration.item.name {
                            locals.push(WithInfo {
                                info: declaration.info.clone(),
                                item: (RenderedSuggestionKind::TypeParameter, name, None),
                            });
                        }
                    }
                }
            }
        }

        for expression in expression_tree {
            if let wipple_driver::typecheck::TypedExpressionKind::Variable(name, path) =
                expression.item.kind
            {
                if let Some(declaration) = self.get_declaration_from_path(&path).await {
                    let r#type = WithInfo {
                        info: expression.info,
                        item: expression.item.r#type,
                    };

                    locals.push(WithInfo {
                        info: declaration.info,
                        item: (RenderedSuggestionKind::Variable, name, Some(r#type)),
                    });
                }
            }
        }

        locals
    }

    pub async fn get_path_at_cursor(
        &self,
        path: &str,
        index: u32,
    ) -> Option<WithInfo<wipple_driver::lower::Path>> {
        self.0
            .lock()
            .unwrap()
            .ide
            .as_ref()?
            .symbols
            .iter()
            .find(|symbol| self.compare_cursor_with_info(path, index, &symbol.info))
            .cloned()
    }

    pub async fn get_expression_at_cursor(
        &self,
        path: &str,
        index: u32,
    ) -> Option<WithInfo<wipple_driver::typecheck::TypedExpression<wipple_driver::Driver>>> {
        self.get_expression_tree_at_cursor(path, index)
            .await?
            .into_iter()
            .next()
    }

    async fn get_expression_tree_at_cursor(
        &self,
        path: &str,
        index: u32,
    ) -> Option<Vec<WithInfo<wipple_driver::typecheck::TypedExpression<wipple_driver::Driver>>>>
    {
        self.0
            .lock()
            .unwrap()
            .libraries
            .iter()
            .flat_map(|library| library.items.values())
            .find_map(|item| {
                if !self.compare_cursor_with_info(path, index, &item.expression.info) {
                    return None;
                }

                let mut candidates = Vec::new();
                self.traverse_expression(&item.expression, &mut |expression| {
                    if index >= expression.info.location.span.start
                        && index < expression.info.location.span.end
                    {
                        candidates.push(expression.map(Clone::clone));
                    }

                    false
                });

                if candidates.is_empty() {
                    return None;
                }

                // Prioritize the most specific expression in the source code
                // (FIXME: Just reverse `candidates`?)
                candidates.sort_by_key(|expression| {
                    expression.info.location.span.end - expression.info.location.span.start
                });

                Some(candidates)
            })
    }

    fn traverse_expression(
        &self,
        expression: &WithInfo<wipple_driver::typecheck::TypedExpression<wipple_driver::Driver>>,
        f: &mut impl FnMut(
            WithInfo<&wipple_driver::typecheck::TypedExpression<wipple_driver::Driver>>,
        ) -> bool,
    ) -> Option<WithInfo<wipple_driver::typecheck::TypedExpression<wipple_driver::Driver>>> {
        fn traverse_expression_inner(
            expression: WithInfo<&wipple_driver::typecheck::TypedExpression<wipple_driver::Driver>>,
            f: &mut impl FnMut(
                WithInfo<&wipple_driver::typecheck::TypedExpression<wipple_driver::Driver>>,
            ) -> bool,
        ) -> ControlFlow<WithInfo<wipple_driver::typecheck::TypedExpression<wipple_driver::Driver>>>
        {
            if f(expression.as_deref()) {
                return ControlFlow::Break(expression.map(Clone::clone));
            }

            match &expression.item.kind {
                wipple_driver::typecheck::TypedExpressionKind::Function { body, .. } => {
                    traverse_expression_inner(body.as_deref(), f)
                }
                wipple_driver::typecheck::TypedExpressionKind::Block { statements, .. } => {
                    for statement in statements {
                        traverse_expression_inner(statement.as_ref(), f)?;
                    }

                    ControlFlow::Continue(())
                }
                wipple_driver::typecheck::TypedExpressionKind::Do(value) => {
                    traverse_expression_inner(value.as_deref(), f)
                }
                wipple_driver::typecheck::TypedExpressionKind::Call {
                    function, inputs, ..
                } => {
                    traverse_expression_inner(function.as_deref(), f)?;

                    for input in inputs {
                        traverse_expression_inner(input.as_ref(), f)?;
                    }

                    ControlFlow::Continue(())
                }
                wipple_driver::typecheck::TypedExpressionKind::When { input, arms, .. } => {
                    traverse_expression_inner(input.as_deref(), f)?;

                    for arm in arms {
                        traverse_expression_inner(arm.item.body.as_ref(), f)?;
                    }

                    ControlFlow::Continue(())
                }
                wipple_driver::typecheck::TypedExpressionKind::Intrinsic { inputs, .. } => {
                    for input in inputs {
                        traverse_expression_inner(input.as_ref(), f)?;
                    }

                    ControlFlow::Continue(())
                }
                wipple_driver::typecheck::TypedExpressionKind::Initialize { value, .. } => {
                    traverse_expression_inner(value.as_deref(), f)
                }
                wipple_driver::typecheck::TypedExpressionKind::Mutate { value, .. } => {
                    traverse_expression_inner(value.as_deref(), f)
                }
                wipple_driver::typecheck::TypedExpressionKind::Structure { fields, .. } => {
                    for field in fields {
                        traverse_expression_inner(field.item.value.as_ref(), f)?;
                    }

                    ControlFlow::Continue(())
                }
                wipple_driver::typecheck::TypedExpressionKind::Variant { values, .. } => {
                    for value in values {
                        traverse_expression_inner(value.as_ref(), f)?;
                    }

                    ControlFlow::Continue(())
                }
                wipple_driver::typecheck::TypedExpressionKind::Wrapper(value) => {
                    traverse_expression_inner(value.as_deref(), f)
                }
                wipple_driver::typecheck::TypedExpressionKind::Tuple(values) => {
                    for value in values {
                        traverse_expression_inner(value.as_ref(), f)?;
                    }

                    ControlFlow::Continue(())
                }
                wipple_driver::typecheck::TypedExpressionKind::Format { segments, .. } => {
                    for segment in segments {
                        traverse_expression_inner(segment.value.as_ref(), f)?;
                    }

                    ControlFlow::Continue(())
                }
                wipple_driver::typecheck::TypedExpressionKind::Marker { .. }
                | wipple_driver::typecheck::TypedExpressionKind::Number { .. }
                | wipple_driver::typecheck::TypedExpressionKind::Unknown(_)
                | wipple_driver::typecheck::TypedExpressionKind::Variable { .. }
                | wipple_driver::typecheck::TypedExpressionKind::Constant { .. }
                | wipple_driver::typecheck::TypedExpressionKind::Trait { .. }
                | wipple_driver::typecheck::TypedExpressionKind::Text { .. } => {
                    ControlFlow::Continue(())
                }
            }
        }

        match traverse_expression_inner(expression.as_ref(), f) {
            ControlFlow::Break(expression) => Some(expression),
            ControlFlow::Continue(()) => None,
        }
    }

    fn compare_info(
        &self,
        left: &wipple_driver::Info,
        right: &wipple_driver::Info,
        between: bool,
    ) -> bool {
        left.location.visible_path == right.location.visible_path
            && if between {
                left.location.span.start >= right.location.span.start
                    && left.location.span.end <= right.location.span.end
            } else {
                left.location.span == right.location.span
            }
    }

    fn compare_cursor_with_info(&self, path: &str, index: u32, info: &wipple_driver::Info) -> bool {
        // HACK: The top level has a span of 0..0
        let is_top_level = info.location.span.start == 0 && info.location.span.end == 0;

        is_top_level
            || (path == info.location.visible_path.as_ref()
                && index >= info.location.span.start
                && index < info.location.span.end)
    }

    fn name_for_path(&self, path: &wipple_driver::lower::Path) -> Option<String> {
        path.0
            .last()
            .and_then(|segment| segment.name())
            .map(ToString::to_string)
    }
}
