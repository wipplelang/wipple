//! Render compiler output as strings.

use crate::util::WithInfo;
use line_index::LineIndex;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize)]
pub struct AnyDeclaration {
    pub name: Option<String>,
    pub path: crate::lower::Path,
    pub kind: AnyDeclarationKind,
}

#[derive(Debug, Clone, Serialize)]
pub enum AnyDeclarationKind {
    Syntax(crate::typecheck::SyntaxDeclaration),
    Type(crate::typecheck::TypeDeclaration),
    Trait(crate::typecheck::TraitDeclaration),
    TypeParameter(crate::typecheck::TypeParameterDeclaration),
    Constant(crate::typecheck::ConstantDeclaration),
    Instance(crate::typecheck::InstanceDeclaration),
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
    pub start: SourceLocation,
    pub end: SourceLocation,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RenderedDiagnostic {
    pub location: RenderedSourceLocation,
    pub severity: RenderedDiagnosticSeverity,
    pub message: String,
    pub description: Option<String>,
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
    pub declaration: String,
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
    Syntax,
    Type,
    Trait,
    TypeParameter,
    Constant,
    Variable,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DescribeOptions {
    NoDescribe,
    DescribeWithoutArticle,
    DescribeWithArticle,
}

#[derive(Clone)]
pub struct Render<'a> {
    pub driver: &'a crate::driver::Driver,
    files: HashMap<String, (crate::driver::File, LineIndex)>,
    declarations: Vec<WithInfo<AnyDeclaration>>,
}

impl crate::driver::Driver {
    pub fn render(&self) -> Render<'_> {
        Render::new(self)
    }
}

impl<'a> Render<'a> {
    pub fn new(driver: &'a crate::driver::Driver) -> Self {
        let mut render = Render {
            driver,
            files: driver
                .interface
                .files
                .iter()
                .map(|file| {
                    (
                        file.path.clone(),
                        (file.clone(), LineIndex::new(&file.code)),
                    )
                })
                .collect(),
            declarations: Vec::new(),
        };

        macro_rules! insert_declaration {
            ($decl:ident($ty:ident)) => {
                render.declarations.extend(
                    driver
                        .interface
                        .$decl
                        .iter()
                        .map(|(path, declaration)| WithInfo {
                            info: declaration.info.clone(),
                            item: AnyDeclaration {
                                name: render.name_for_path(&path),
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
            syntax_declarations(Syntax),
            type_declarations(Type),
            trait_declarations(Trait),
            type_parameter_declarations(TypeParameter),
            constant_declarations(Constant),
            instance_declarations(Instance),
        );

        render
    }

    pub fn get_declaration_from_name(&self, name: &str) -> Option<WithInfo<AnyDeclaration>> {
        self.declarations
            .iter()
            .filter(|declaration| {
                matches!(
                    declaration.item.kind,
                    AnyDeclarationKind::Type(_)
                        | AnyDeclarationKind::Trait(_)
                        | AnyDeclarationKind::Constant(_)
                        | AnyDeclarationKind::Instance(_)
                )
            })
            .find(|declaration| declaration.item.name.as_deref() == Some(name))
            .cloned()
    }

    pub fn get_declaration_from_path(
        &self,
        path: &crate::lower::Path,
    ) -> Option<WithInfo<AnyDeclaration>> {
        let mut path = path.clone();

        // Resolve the actual declaration the constructor is for
        if let Some(crate::lower::PathComponent::Constructor(_)) = path.last() {
            path.pop();
        }

        self.declarations
            .iter()
            .find(|declaration| declaration.item.path == path)
            .cloned()
    }

    pub fn get_declaration_from_info(
        &self,
        info: &crate::syntax::Location,
        between: bool,
    ) -> Option<WithInfo<AnyDeclaration>> {
        self.declarations
            .iter()
            .find(|declaration| self.compare_info(&declaration.info, info, between))
            .cloned()
    }

    pub fn get_declaration_for_syntax(&self, syntax: &str) -> Option<WithInfo<AnyDeclaration>> {
        self.declarations
            .iter()
            .find(|declaration| {
                declaration.item.name.as_deref() == Some(syntax)
                    && matches!(
                        declaration.item.path.last(),
                        Some(crate::lower::PathComponent::Syntax(_))
                    )
            })
            .cloned()
    }

    pub fn get_instances_for_trait(
        &self,
        r#trait: &crate::lower::Path,
    ) -> Vec<WithInfo<AnyDeclaration>> {
        self.declarations
            .iter()
            .filter(|declaration| match &declaration.item.kind {
                AnyDeclarationKind::Instance(instance) => {
                    instance.instance.item.r#trait == *r#trait && !instance.default
                }
                _ => false,
            })
            .cloned()
            .collect()
    }

    pub fn render_source<T>(&self, value: &WithInfo<T>) -> Option<String> {
        let (file, _) = self.files.get(value.info.path.as_ref())?;
        Some(file.code.clone())
    }

    pub fn render_source_location<T>(&self, value: &WithInfo<T>) -> Option<RenderedSourceLocation> {
        let (file, line_index) = self.files.get(value.info.path.as_ref())?;

        let start_location =
            line_index.try_line_col(line_index::TextSize::new(value.info.span.start))?;

        let end_location =
            line_index.try_line_col(line_index::TextSize::new(value.info.span.end))?;

        Some(RenderedSourceLocation {
            path: file.path.clone(),
            start: SourceLocation {
                line: start_location.line,
                column: start_location.col,
                index: value.info.span.start,
            },
            end: SourceLocation {
                line: end_location.line,
                column: end_location.col,
                index: value.info.span.end,
            },
        })
    }

    pub fn render_code<T>(&self, value: &WithInfo<T>) -> Option<String> {
        let rendered_source_location = self.render_source_location(value)?;

        let inner = self;
        let (file, _) = inner.files.get(&rendered_source_location.path)?;

        Some(
            file.code[rendered_source_location.start.index as usize
                ..rendered_source_location.end.index as usize]
                .to_string(),
        )
    }

    pub fn render_declaration(
        &self,
        declaration: &WithInfo<AnyDeclaration>,
        full: bool,
    ) -> Option<String> {
        match &declaration.item.kind {
            AnyDeclarationKind::Syntax(_) => {
                let name = declaration.item.name.as_deref().unwrap_or("<unknown>");
                Some(name.to_string())
            }
            AnyDeclarationKind::Type(type_declaration) => {
                let type_function = self.render_type_function(&type_declaration.parameters, &[]);

                Some(format!(
                    "{} : {}type",
                    declaration.item.name.as_deref().unwrap_or("<unknown>"),
                    type_function,
                ))
            }
            AnyDeclarationKind::Trait(trait_declaration) => {
                let type_function = self.render_type_function(&trait_declaration.parameters, &[]);

                Some(format!(
                    "{} : {}trait",
                    declaration.item.name.as_deref().unwrap_or("<unknown>"),
                    type_function,
                ))
            }
            AnyDeclarationKind::TypeParameter(_) => declaration.item.name.clone(),
            AnyDeclarationKind::Constant(constant_declaration) => {
                let name = declaration.item.name.as_deref().unwrap_or("<unknown>");

                let r#type = self.render_type(
                    &constant_declaration.r#type,
                    true,
                    DescribeOptions::NoDescribe,
                    false,
                );

                let type_function = if full {
                    self.render_type_function(
                        &constant_declaration.parameters,
                        &constant_declaration.bounds,
                    )
                } else {
                    String::new()
                };

                Some(format!("{} :: {}{}", name, type_function, r#type))
            }
            AnyDeclarationKind::Instance(instance_declaration) => {
                let type_function = if full {
                    self.render_type_function(
                        &instance_declaration.parameters,
                        &instance_declaration.bounds,
                    )
                } else {
                    String::new()
                };

                let instance = self.render_instance(&instance_declaration.instance, false);

                Some(format!("{}instance {}", type_function, instance))
            }
        }
    }

    pub fn render_pattern(
        &self,
        pattern: &crate::typecheck::exhaustiveness::Pattern,
        is_top_level: bool,
    ) -> String {
        match pattern {
            crate::typecheck::exhaustiveness::Pattern::Constructor(constructor, values) => {
                match constructor {
                    crate::typecheck::exhaustiveness::Constructor::Variant(path) => {
                        let name = match path.last() {
                            Some(crate::lower::PathComponent::Variant(name)) => name,
                            _ => return String::from("<unknown>"),
                        };

                        let rendered = if values.is_empty() {
                            name.to_string()
                        } else {
                            let values = values
                                .iter()
                                .map(|pattern| self.render_pattern(pattern, false))
                                .collect::<Vec<_>>()
                                .join(" ");

                            format!("{} {}", name, values)
                        };

                        if is_top_level || values.is_empty() {
                            rendered
                        } else {
                            format!("({})", rendered)
                        }
                    }
                    crate::typecheck::exhaustiveness::Constructor::Tuple => {
                        let rendered = if values.is_empty() {
                            String::from("()")
                        } else if values.len() == 1 {
                            format!("{} ;", self.render_pattern(values.first().unwrap(), false),)
                        } else {
                            values
                                .iter()
                                .map(|pattern| self.render_pattern(pattern, false))
                                .collect::<Vec<_>>()
                                .join(" ; ")
                        };

                        if is_top_level || values.is_empty() {
                            rendered
                        } else {
                            format!("({})", rendered)
                        }
                    }
                    crate::typecheck::exhaustiveness::Constructor::Structure => {
                        String::from("{...}")
                    }
                    crate::typecheck::exhaustiveness::Constructor::Wrapper(path) => {
                        let declaration = match self.get_declaration_from_path(path) {
                            Some(declarataion) => declarataion,
                            None => return String::from("<unknown>"),
                        };

                        let name = declaration.item.name.as_deref().unwrap_or("<unknown>");

                        let value = self.render_pattern(values.first().unwrap(), false);

                        if is_top_level {
                            format!("{} {}", name, value)
                        } else {
                            format!("({} {})", name, value)
                        }
                    }
                    crate::typecheck::exhaustiveness::Constructor::Unbounded => String::from("_"),
                }
            }
            crate::typecheck::exhaustiveness::Pattern::Binding => String::from("_"),
            crate::typecheck::exhaustiveness::Pattern::Or(patterns) => {
                let rendered = patterns
                    .iter()
                    .map(|pattern| self.render_pattern(pattern, false))
                    .collect::<Vec<_>>()
                    .join(" or ");

                if is_top_level {
                    rendered
                } else {
                    format!("({})", rendered)
                }
            }
        }
    }

    pub fn render_type(
        &self,
        r#type: &WithInfo<crate::typecheck::Type>,
        is_top_level: bool,
        describe: DescribeOptions,
        render_as_code: bool,
    ) -> String {
        if is_top_level && !matches!(describe, DescribeOptions::NoDescribe) {
            #[derive(Default)]
            struct Modifiers {
                no_grammar: bool,
            }

            let message = (|| {
                let result =
                    self.driver
                        .resolve_attribute_like_trait("describe-type", r#type.clone(), 1)?;

                fn extract_message_with_modifiers(
                    r#type: WithInfo<crate::typecheck::Type>,
                    driver: &crate::driver::Driver,
                    modifiers: &mut Modifiers,
                ) -> Option<crate::typecheck::CustomMessage> {
                    match r#type.item {
                        crate::typecheck::Type::Message { segments, trailing } => {
                            Some(crate::typecheck::CustomMessage { segments, trailing })
                        }
                        crate::typecheck::Type::Declared { path, parameters }
                            if driver.type_is_language_item(&path, "no-grammar") =>
                        {
                            modifiers.no_grammar = true;

                            extract_message_with_modifiers(
                                parameters.into_iter().next()?,
                                driver,
                                modifiers,
                            )
                        }
                        _ => None,
                    }
                }

                let mut modifiers = Modifiers::default();
                let message = extract_message_with_modifiers(
                    result.into_iter().next()?,
                    self.driver,
                    &mut modifiers,
                )?;

                Some((message, modifiers))
            })();

            if let Some((message, modifiers)) = message {
                let add_article = !modifiers.no_grammar
                    && matches!(describe, DescribeOptions::DescribeWithArticle);

                let message = self.render_custom_message(&message, add_article, false);

                return if add_article {
                    self.add_article_prefix(&message)
                } else {
                    message
                };
            }
        }

        fn render_type_inner(
            render: &Render<'_>,
            r#type: WithInfo<&crate::typecheck::Type>,
            is_top_level: bool,
        ) -> String {
            match &r#type.item {
                crate::typecheck::Type::Unknown => String::from("_"),
                crate::typecheck::Type::Parameter(parameter) => render
                    .name_for_path(parameter)
                    .unwrap_or_else(|| String::from("<unknown>")),
                crate::typecheck::Type::Declared { path, parameters } => {
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
                                ))
                                .collect::<Vec<_>>()
                                .join(" "),
                        )
                    };

                    if is_top_level || parameters.is_empty() {
                        rendered
                    } else {
                        format!("({})", rendered)
                    }
                }
                crate::typecheck::Type::Function { inputs, output } => {
                    let inputs = inputs
                        .iter()
                        .map(|input| render_type_inner(render, input.as_ref(), false))
                        .collect::<Vec<_>>()
                        .join(" ");

                    let output = render_type_inner(render, output.as_deref(), true);

                    let rendered = format!("{} -> {}", inputs, output);

                    if is_top_level {
                        rendered
                    } else {
                        format!("({})", rendered)
                    }
                }
                crate::typecheck::Type::Tuple(elements) => {
                    let rendered = if elements.is_empty() {
                        String::new()
                    } else if elements.len() == 1 {
                        format!(
                            "{} ;",
                            render_type_inner(render, elements.first().unwrap().as_ref(), false,),
                        )
                    } else {
                        elements
                            .iter()
                            .map(|r#type| render_type_inner(render, r#type.as_ref(), false))
                            .collect::<Vec<_>>()
                            .join(" ; ")
                    };

                    if is_top_level {
                        rendered
                    } else {
                        format!("({})", rendered)
                    }
                }
                crate::typecheck::Type::Block(value) => {
                    format!("{{{}}}", render_type_inner(render, value.as_deref(), true,))
                }
                crate::typecheck::Type::Intrinsic => String::from("intrinsic"),
                crate::typecheck::Type::Message { segments, trailing } => {
                    let mut message = String::new();
                    let mut inputs = String::new();

                    for segment in segments {
                        message.push_str(&segment.text);
                        message.push('_');

                        inputs.push(' ');
                        inputs.push_str(&render_type_inner(render, segment.r#type.as_ref(), false));
                    }

                    message.push_str(trailing);

                    if is_top_level || inputs.is_empty() {
                        format!("{:?}{}", message, inputs)
                    } else {
                        format!("({:?}{})", message, inputs)
                    }
                }
                crate::typecheck::Type::Equal { left, right } => {
                    let rendered = format!(
                        "{} = {}",
                        render_type_inner(render, left.as_deref(), false),
                        render_type_inner(render, right.as_deref(), false),
                    );

                    if is_top_level {
                        rendered
                    } else {
                        format!("({})", rendered)
                    }
                }
            }
        }

        let rendered = render_type_inner(self, r#type.as_ref(), is_top_level);

        if render_as_code {
            format!("`{}`", rendered)
        } else {
            rendered
        }
    }

    pub fn render_type_function(
        &self,
        parameters: &[crate::lower::Path],
        bounds: &[WithInfo<crate::typecheck::Instance>],
    ) -> String {
        let rendered_parameters = parameters
            .iter()
            .filter_map(|parameter| {
                let name = self.name_for_path(parameter)?;

                let declaration = match self.get_declaration_from_path(parameter)?.item.kind {
                    AnyDeclarationKind::TypeParameter(declaration) => declaration,
                    _ => return Some(name),
                };

                Some(match (declaration.infer, declaration.default) {
                    (Some(_), Some(default)) => {
                        let default =
                            self.render_type(&default, true, DescribeOptions::NoDescribe, false);
                        format!("(infer {name} : {default})")
                    }
                    (Some(_), None) => format!("(infer {name})"),
                    (None, Some(default)) => {
                        let default =
                            self.render_type(&default, true, DescribeOptions::NoDescribe, false);
                        format!("({name} : {default})")
                    }
                    (None, None) => name,
                })
            })
            .collect::<Vec<_>>()
            .join(" ");

        let rendered_bounds = bounds
            .iter()
            .map(|bound| self.render_instance(bound, false))
            .collect::<Vec<_>>()
            .join(" ");

        match (rendered_parameters.is_empty(), rendered_bounds.is_empty()) {
            (true, true) => String::new(),
            (true, false) => format!("_ where {} => ", rendered_bounds),
            (false, true) => format!("{} => ", rendered_parameters),
            (false, false) => format!("{} where {} => ", rendered_parameters, rendered_bounds),
        }
    }

    pub fn render_instance(
        &self,
        instance: &WithInfo<crate::typecheck::Instance>,
        is_top_level: bool,
    ) -> String {
        let r#trait = self
            .name_for_path(&instance.item.r#trait)
            .unwrap_or_else(|| String::from("<unknown>"));

        let parameters = instance
            .item
            .parameters
            .iter()
            .map(|r#type| self.render_type(r#type, false, DescribeOptions::NoDescribe, false))
            .collect::<Vec<_>>();

        if parameters.is_empty() {
            r#trait
        } else if is_top_level {
            format!("{} {}", r#trait, parameters.join(" "))
        } else {
            format!("({} {})", r#trait, parameters.join(" "))
        }
    }

    pub fn render_custom_message(
        &self,
        message: &crate::typecheck::CustomMessage,
        will_already_add_article: bool,
        render_as_code: bool,
    ) -> String {
        let render_segments_as_code = message.segments.first().map_or_else(
            || message.trailing.starts_with('`'),
            |segment| segment.text.starts_with('`'),
        ) && message.trailing.ends_with('`');

        let mut result = String::new();
        for segment in &message.segments {
            let code = if render_segments_as_code || segment.text.ends_with('`') {
                self.render_code(&segment.r#type)
            } else {
                None
            };

            result.push_str(&segment.text);

            result.push_str(&match code {
                Some(code) => code,
                None => self.render_type(
                    &segment.r#type,
                    true,
                    if will_already_add_article {
                        DescribeOptions::DescribeWithoutArticle
                    } else {
                        DescribeOptions::DescribeWithArticle
                    },
                    true,
                ),
            });
        }

        result.push_str(&message.trailing);

        if render_segments_as_code && !render_as_code {
            result = result[1..result.len() - 1].to_string();
        }

        result
    }

    pub fn render_diagnostic(
        &self,
        diagnostic: &WithInfo<crate::driver::Diagnostic>,
    ) -> Option<RenderedDiagnostic> {
        let rendered_source_location =
            self.render_source_location(diagnostic)
                .unwrap_or_else(|| RenderedSourceLocation {
                    path: diagnostic.info.path.to_string(),
                    start: Default::default(),
                    end: Default::default(),
                });

        let severity;
        let message;
        let description;
        match &diagnostic.item {
            crate::driver::Diagnostic::Tokenize(tokenize_diagnostic) => match tokenize_diagnostic {
                crate::syntax::tokenize::Diagnostic::InvalidToken => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = String::from("Unrecognized symbol");
                    description = Some(String::from(
                        "This symbol isn't valid in Wipple code. Try removing it.",
                    ));
                }
                crate::syntax::tokenize::Diagnostic::EmptyParentheses => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = String::from("Missing code between the parentheses");
                    description = Some(String::from(
                        "Try putting something between the opening `(` and the closing `)`, or remove the parentheses.",
                    ));
                }
                crate::syntax::tokenize::Diagnostic::EmptyBraces => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = String::from("Missing code between the braces");
                    description = Some(String::from(
                        "Try putting `None` between the opening `{` and the closing `}` to indicate that this block does nothing.",
                    ));
                }
                crate::syntax::tokenize::Diagnostic::Mismatch {
                    expected, found, ..
                } => {
                    severity = RenderedDiagnosticSeverity::Error;

                    match (expected, found) {
                        (Some(expected), Some(found)) => {
                            message = String::from("Expected a different symbol here");
                            description = Some(format!(
                                "You provided a `{}`, but you need to put a `{}` here instead.",
                                self.render_token(found),
                                self.render_token(expected)
                            ));
                        }
                        (Some(expected), None) => match expected {
                            crate::syntax::tokenize::Token::RightParenthesis => {
                                message = String::from("Missing closing `)`");
                                description = Some(String::from(
                                    "Every opening parenthesis needs a closing parenthesis. Try adding one at the end.",
                                ));
                            }
                            crate::syntax::tokenize::Token::RightBracket => {
                                message = String::from("Missing closing `]`");
                                description = Some(String::from(
                                    "Every opening bracket needs a closing bracket. Try adding one at the end.",
                                ));
                            }
                            crate::syntax::tokenize::Token::RightBrace => {
                                message = String::from("Missing closing `}`");
                                description = Some(String::from(
                                    "Every opening brace needs a closing brace. Try adding one at the end.",
                                ));
                            }
                            _ => {
                                message = format!("Missing `{}` here", self.render_token(expected));
                                description = Some(String::from(
                                    "Try adding this symbol, or double-check your parentheses.",
                                ));
                            }
                        },
                        (None, Some(found)) => match found {
                            crate::syntax::tokenize::Token::RightParenthesis => {
                                message = String::from("Extra closing `)`");
                                description = Some(String::from(
                                    "Make sure you have an opening `(` in the right place, or remove this one.",
                                ));
                            }
                            crate::syntax::tokenize::Token::RightBracket => {
                                message = String::from("Extra closing `]`");
                                description = Some(String::from(
                                    "Make sure you have an opening `[` in the right place, or remove this one.",
                                ));
                            }
                            crate::syntax::tokenize::Token::RightBrace => {
                                message = String::from("Extra closing `}`");
                                description = Some(String::from(
                                    "Make sure you have an opening `{` in the right place, or remove this one.",
                                ));
                            }
                            _ => {
                                message = format!("Extra `{}`", self.render_token(found));
                                description = Some(format!(
                                    "Make sure this `{}` is in the right place, or remove it.",
                                    self.render_token(found)
                                ));
                            }
                        },
                        (None, None) => return None,
                    }
                }
            },
            crate::driver::Diagnostic::Parse(parse_diagnostic) => {
                match &parse_diagnostic.direction {
                    Some(crate::syntax::parse::Direction::Before(before)) => {
                        severity = RenderedDiagnosticSeverity::Error;
                        message = format!(
                            "Unexpected {} before {}",
                            self.render_syntax_kind(&parse_diagnostic.expected),
                            self.render_syntax_kind(before)
                        );
                        description = Some(format!(
                            "Double-check your parentheses, or remove the {}.",
                            self.render_syntax_kind(&parse_diagnostic.expected)
                        ));
                    }
                    Some(crate::syntax::parse::Direction::After(after)) => {
                        severity = RenderedDiagnosticSeverity::Error;
                        message = format!(
                            "Unexpected {} after {}",
                            self.render_syntax_kind(&parse_diagnostic.expected),
                            self.render_syntax_kind(after)
                        );
                        description = Some(format!(
                            "Double-check your parentheses, or remove the {}.",
                            self.render_syntax_kind(&parse_diagnostic.expected)
                        ));
                    }
                    None => {
                        severity = RenderedDiagnosticSeverity::Error;
                        message = format!(
                            "Missing `{}` here",
                            self.render_syntax_kind(&parse_diagnostic.expected)
                        );
                        description = Some(String::from(
                            "Try adding this symbol, or double-check your parentheses.",
                        ));
                    }
                }
            }
            crate::driver::Diagnostic::Syntax(syntax_diagnostic) => match syntax_diagnostic {
                crate::syntax::Diagnostic::UnexpectedBound => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = String::from("Unexpected bound here");
                    description = Some(String::from(
                        "Bounds aren't allowed on type and trait definitions.",
                    ));
                }
                crate::syntax::Diagnostic::ExpectedConstantValue(value) => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = format!("Missing value for `{}`", value);
                    description = Some(String::from(
                        "You created a constant using `::`, but it's missing a value. Try adding a value on the next line using the `:`, or double-check that you intended to use `::` here.",
                    ));
                }
                crate::syntax::Diagnostic::EmptyTypeRepresentation => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message =
                        String::from("Missing fields or variants between the `{ }` for this type");
                    description = Some(String::from(
                        "If you're trying to create a type without any fields, remove the braces.",
                    ));
                }
                crate::syntax::Diagnostic::ExpectedField => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = String::from("Expected a field here");
                    description = Some(String::from(
                        "You're creating a structure type, which needs to be made entirely of fields using `::`.",
                    ));
                }
                crate::syntax::Diagnostic::ExpectedVariant => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = String::from("Expected a variant here");
                    description = Some(String::from(
                        "You're creating an enumeration type, which needs to be made entirely of variants. You can't mix in fields with `::`.",
                    ));
                }
                crate::syntax::Diagnostic::InvalidTextLiteral(error) => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = format!("Invalid text: {}", error.error);
                    description = Some(String::from(
                        "Some characters aren't allowed inside text, or you might be using an invalid escape sequence.",
                    ));
                }
                crate::syntax::Diagnostic::InvalidPlaceholderText { expected, found } => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = format!(
                        "This text needs {} inputs, but you provided {}",
                        expected, found
                    );
                    description = Some(String::from(
                        "You need to provide the same number of items as there are `_` placeholders inside the text. Make sure you're putting your parentheses in the right places.",
                    ));
                }
            },
            crate::driver::Diagnostic::Lower(lower_diagnostic) => match lower_diagnostic {
                crate::lower::Diagnostic::UnresolvedName(name) => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = format!("Couldn't find `{}`", name);
                    description = Some(String::from(
                        "Double-check your code for spelling mistakes.",
                    ));
                }
                crate::lower::Diagnostic::UnresolvedType(name) => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = format!("Couldn't find a type named `{}`", name);
                    description = Some(String::from(
                        "Double-check your code for spelling mistakes.",
                    ));
                }
                crate::lower::Diagnostic::UnresolvedTrait(name) => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = format!("Couldn't find a trait named `{}`", name);
                    description = Some(String::from(
                        "Double-check your code for spelling mistakes.",
                    ));
                }
                crate::lower::Diagnostic::UnresolvedVariant(name) => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = format!("Couldn't find a variant named `{}`", name);
                    description = Some(String::from(
                        "Double-check your code for spelling mistakes.",
                    ));
                }
                crate::lower::Diagnostic::UnresolvedLanguageItem(name) => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = String::from("Couldn't process this code");
                    description = Some(format!(
                        "You've found a bug in Wipple — your code is correct, but it uses the `{}` language item, which hasn't been defined. Please report feedback so this can be fixed!",
                        name
                    ));
                }
                crate::lower::Diagnostic::AmbiguousName { name, .. } => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = format!("`{}` has multiple definitions", name);
                    description = Some(format!(
                        "You can't use `{}` here because it could refer to two or more different values.",
                        name
                    ));
                }
                crate::lower::Diagnostic::AlreadyDefined(name) => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = format!("`{}` is already defined", name);
                    description = Some(String::from(
                        "This name is already chosen. You'll have to choose a different one.",
                    ));
                }
                crate::lower::Diagnostic::NestedLanguageDeclaration => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = String::from("Language items must be declared at the top level");
                    description = Some(String::from(
                        "Wipple already defines all the language items you need, so you don't need to define any yourself. Double-check your code and make sure it's doing what you expect.",
                    ));
                }
                crate::lower::Diagnostic::NotAWrapper => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = String::from(
                        "This pattern is supposed to match a wrapper type, but it actually matches a structure or enumeration type",
                    );
                    description = Some(String::from(
                        "Double-check your parentheses and the type of the input you're matching.",
                    ));
                }
                crate::lower::Diagnostic::WrapperExpectsASinglePattern => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message =
                        String::from("Expected a single pattern here, but found more than one");
                    description = Some(String::from(
                        "Double-check your parentheses and make sure you're using the right pattern here.",
                    ));
                }
                crate::lower::Diagnostic::InvalidMutatePattern => {
                    severity = RenderedDiagnosticSeverity::Error;
                    message = String::from("Can't use `!` here");
                    description = Some(String::from(
                        "You can only use `!` to mutate an existing variable on the left-hand side of the `:`. Putting `!` inside complex patterns isn't supported yet.",
                    ));
                }
                crate::lower::Diagnostic::MissingTypes(count) => {
                    let code = self.render_code(diagnostic).unwrap_or_default();

                    severity = RenderedDiagnosticSeverity::Error;
                    message = format!("Missing {} type parameters here", count);
                    description = Some(format!(
                        "You might be missing parentheses to group `{}` with any types after it.",
                        code
                    ));
                }
                crate::lower::Diagnostic::ExtraType => {
                    let code = self.render_code(diagnostic).unwrap_or_default();

                    severity = RenderedDiagnosticSeverity::Error;
                    message = String::from("Extra type parameter here");
                    description = Some(format!(
                        "This type doesn't need that many inputs. Try removing `{}` or moving it to a new line.",
                        code
                    ));
                }
            },
            crate::driver::Diagnostic::Typecheck(typecheck_diagnostic) => {
                match typecheck_diagnostic {
                    crate::typecheck::Diagnostic::RecursionLimit => {
                        severity = RenderedDiagnosticSeverity::Error;
                        message = String::from("This code is too complex to check");
                        description = Some(String::from(
                            "Wipple ran out of time while checking this code. Try breaking it into smaller pieces or adding more type annotations using `::`.",
                        ));
                    }
                    crate::typecheck::Diagnostic::MissingLanguageItem(name) => {
                        severity = RenderedDiagnosticSeverity::Error;
                        message = String::from("Couldn't process this code");
                        description = Some(format!(
                            "You've found a bug in Wipple — your code is correct, but Wipple couldn't find the `{}` language item for it. Please report feedback so this can be fixed!",
                            name
                        ));
                    }
                    crate::typecheck::Diagnostic::UnknownType(r#type) => {
                        severity = RenderedDiagnosticSeverity::Error;

                        if matches!(r#type, crate::typecheck::Type::Unknown) {
                            let code = self.render_code(diagnostic).unwrap_or_default();
                            message = String::from("Could not determine the meaning of this code");
                            description = Some(format!(
                                "Wipple needs more information before it can run this code. Try assigning `{}` to a variable using `:`, and then use it somewhere else in the program, to help Wipple determine its meaning.",
                                code
                            ));
                        } else {
                            let rendered_type = self.render_type(
                                &WithInfo {
                                    info: diagnostic.info.clone(),
                                    item: r#type.clone(),
                                },
                                true,
                                DescribeOptions::NoDescribe,
                                true,
                            );

                            message = String::from("Could not determine the meaning of this code");
                            description = Some(format!(
                                "Wipple needs more information before it can run this code. Its type is {}, but the `_` placeholders are unknown.",
                                rendered_type
                            ));
                        }
                    }
                    crate::typecheck::Diagnostic::UndeclaredTypeParameter(name) => {
                        severity = RenderedDiagnosticSeverity::Error;
                        message = format!(
                            "Can't use `{}` here because it's from an outer function",
                            name
                        );
                        description = Some(String::from(
                            "You can't use a type parameter that isn't declared directly within this function's type annotation. Try adding another type parameter to this function and add a type annotation where it's used if needed.",
                        ));
                    }
                    crate::typecheck::Diagnostic::Mismatch {
                        actual, expected, ..
                    } => {
                        severity = RenderedDiagnosticSeverity::Error;

                        let mut expected_message = self.render_type(
                            expected,
                            true,
                            DescribeOptions::DescribeWithArticle,
                            true,
                        );

                        let mut actual_message = self.render_type(
                            actual,
                            true,
                            DescribeOptions::DescribeWithArticle,
                            true,
                        );

                        // If the type descriptions are equal, try rendering the actual type by setting
                        // `describe` to false
                        if expected_message == actual_message {
                            expected_message =
                                self.render_type(expected, true, DescribeOptions::NoDescribe, true);
                            actual_message =
                                self.render_type(actual, true, DescribeOptions::NoDescribe, true);
                        }

                        message = format!(
                            "This code is supposed to be {}, but it's actually {}",
                            actual_message, expected_message
                        );
                        description = Some(format!(
                            "You provided {}, but you need to put {} here instead.",
                            actual_message, expected_message
                        ));
                    }
                    crate::typecheck::Diagnostic::MissingInputs(inputs) => {
                        let code = self.render_code(diagnostic).unwrap_or_default();

                        let inputs = inputs
                            .iter()
                            .map(|r#type| {
                                self.render_type(
                                    r#type,
                                    true,
                                    DescribeOptions::DescribeWithArticle,
                                    true,
                                )
                            })
                            .collect::<Vec<_>>();

                        severity = RenderedDiagnosticSeverity::Error;

                        let inputs_message = match inputs.len() {
                            1 => inputs.into_iter().next().unwrap(),
                            2 => format!("{} and {}", inputs[0], inputs[1]),
                            _ => format!(
                                "{}, and {}",
                                inputs[..inputs.len() - 1].join(", "),
                                inputs[inputs.len() - 1],
                            ),
                        };

                        message = format!("`{}` is missing {}", code, inputs_message);
                        description = Some(String::from(
                            "Try adding these inputs. If you've already provided them, make sure they're all on one line.",
                        ));
                    }
                    crate::typecheck::Diagnostic::ExtraInput => {
                        let code = self.render_code(diagnostic).unwrap_or_default();

                        severity = RenderedDiagnosticSeverity::Error;
                        message = format!("Extra input to `{}`", code);
                        description = Some(format!(
                            "The `{}` function doesn't need that many inputs. Try removing this input or moving it to a new line.",
                            code
                        ));
                    }
                    crate::typecheck::Diagnostic::UnresolvedInstance { instance, .. } => {
                        let code = self.render_code(diagnostic).unwrap_or_default();

                        let rendered_instance = self.render_instance(
                            &WithInfo {
                                info: diagnostic.info.clone(),
                                item: instance.clone(),
                            },
                            true,
                        );

                        severity = RenderedDiagnosticSeverity::Error;
                        message = format!(
                            "Using `{}` requires that `instance ({})` exists",
                            code, rendered_instance
                        );
                        description = Some(format!(
                            "`{}` needs this instance to exist so it can use its input correctly. Make sure you're providing the right type of input here, or you can define your own `instance`.",
                            code
                        ));
                    }
                    crate::typecheck::Diagnostic::TraitHasNoValue(_) => {
                        let code = self.render_code(diagnostic).unwrap_or_default();

                        severity = RenderedDiagnosticSeverity::Error;
                        message = format!("Can't use `{}` as a value", code);
                        description = Some(format!(
                            "`{}` can only be used in `where` bounds, not as a value.",
                            code
                        ));
                    }
                    crate::typecheck::Diagnostic::ExpectedInstanceValue => {
                        severity = RenderedDiagnosticSeverity::Error;
                        message = String::from("Missing a value for this instance");
                        description = Some(String::from("Try adding a value using `:`."));
                    }
                    crate::typecheck::Diagnostic::UnexpectedInstanceValue => {
                        let code = self.render_code(diagnostic).unwrap_or_default();

                        severity = RenderedDiagnosticSeverity::Error;
                        message = String::from("This instance doesn't need a value");
                        description = Some(format!(
                            "You provided a value here using `:`, but the corresponding trait doesn't need a value. Try removing the value and the `:`, leaving just `{}`.",
                            code
                        ));
                    }
                    crate::typecheck::Diagnostic::NotAStructure(r#type) => {
                        let rendered_type = self.render_type(
                            r#type,
                            true,
                            DescribeOptions::DescribeWithArticle,
                            true,
                        );

                        let expected_message = self.render_type(
                            r#type,
                            true,
                            DescribeOptions::DescribeWithArticle,
                            true,
                        );

                        severity = RenderedDiagnosticSeverity::Error;
                        message = format!(
                            "This code is supposed to be {}, but it's actually a structure",
                            rendered_type
                        );
                        description = Some(format!(
                            "You provided a structure, but you need to put {} here instead.",
                            expected_message
                        ));
                    }
                    crate::typecheck::Diagnostic::MissingFields(r#type) => {
                        let rendered_fields = r#type
                            .iter()
                            .map(|field| format!("`{}`", field))
                            .collect::<Vec<_>>()
                            .join(", ");

                        severity = RenderedDiagnosticSeverity::Error;
                        message = format!("Missing values for {}", rendered_fields);
                        description = Some(String::from(
                            "Try adding values for these fields using `:`.",
                        ));
                    }
                    crate::typecheck::Diagnostic::ExtraField => {
                        severity = RenderedDiagnosticSeverity::Error;
                        message = String::from("Extra field ");
                        description = Some(String::from(
                            "Make sure you spelled this field correctly, or remove it.",
                        ));
                    }
                    crate::typecheck::Diagnostic::OverlappingInstances { .. } => {
                        let code = self.render_code(diagnostic).unwrap_or_default();

                        severity = RenderedDiagnosticSeverity::Error;
                        message = format!("`{}` already exists", code);
                        description = Some(String::from(
                            "You can't define two instances that match the same types.",
                        ));
                    }
                    crate::typecheck::Diagnostic::MissingPatterns(patterns) => {
                        let last = patterns.last().unwrap();

                        severity = RenderedDiagnosticSeverity::Error;
                        if patterns.len() == 1 {
                            if matches!(last, crate::typecheck::exhaustiveness::Pattern::Binding) {
                                let code = self.render_code(diagnostic).unwrap_or_default();
                                message =
                                    String::from("Missing a `_` pattern to match remaining inputs");
                                description = Some(format!(
                                    "Try adding one more pattern for `_` to match anything not already matched in `{}`.",
                                    code
                                ));
                            } else {
                                message = format!(
                                    "`when` won't match if it receives `{}`",
                                    self.render_pattern(last, true)
                                );
                                description = Some(format!(
                                    "If the input matches `{}`, `when` won't be able to handle it.",
                                    self.render_pattern(last, true)
                                ));
                            }
                        } else {
                            let patterns_message = format!(
                                "`{}` or `{}`",
                                patterns[..patterns.len() - 1]
                                    .iter()
                                    .map(|pattern| self.render_pattern(pattern, true))
                                    .collect::<Vec<_>>()
                                    .join(", "),
                                self.render_pattern(last, true),
                            );

                            message =
                                format!("`when` won't match if it receives {}", patterns_message);
                            description = Some(format!(
                                "If the input matches {}, `when` won't be able to handle it.",
                                patterns_message
                            ));
                        }
                    }
                    crate::typecheck::Diagnostic::ExtraPattern => {
                        severity = RenderedDiagnosticSeverity::Warning;
                        message = String::from("Extra pattern");
                        description = Some(String::from(
                            "This pattern will never be matched because another pattern above it matches the input already.",
                        ));
                    }
                    crate::typecheck::Diagnostic::Custom {
                        message: custom_message,
                        description: custom_description,
                    } => {
                        severity = RenderedDiagnosticSeverity::Error;
                        message = self.render_custom_message(custom_message, false, true);
                        description = custom_description.as_ref().map(|description| {
                            self.render_custom_message(description, false, true)
                        });
                    }
                }
            }
            crate::driver::Diagnostic::Ir => {
                severity = RenderedDiagnosticSeverity::Error;
                message = String::from("Couldn't process this code");
                description = Some(String::from(
                    "You've found a bug in Wipple — your code is correct, but Wipple couldn't produce IR for it. Please report feedback so this can be fixed!",
                ));
            }
        }

        Some(RenderedDiagnostic {
            location: rendered_source_location,
            severity,
            message,
            description,
        })
    }

    pub fn render_token(&self, token: &crate::syntax::tokenize::Token<'_>) -> String {
        use crate::syntax::tokenize::Token;

        match token {
            Token::Number(_) => self.add_article_prefix("number"),
            Token::LeftParenthesis => self.add_article_prefix("opening parenthesis (`(`)"),
            Token::RightParenthesis => self.add_article_prefix("closing parenthesis (`)`)"),
            Token::LeftBracket => self.add_article_prefix("opening bracket (`[`)"),
            Token::RightBracket => self.add_article_prefix("closing bracket (`]`)"),
            Token::LeftBrace => self.add_article_prefix("opening brace (`{{`)"),
            Token::RightBrace => self.add_article_prefix("closing brace (`}}`)"),
            Token::LineBreak => String::from("the end of the line"),
            Token::Comment(_) => self.add_article_prefix("comment"),
            Token::Keyword(value) => format!("the word `{}`", value),
            Token::Operator(value) => format!("the symbol `{}`", value),
            Token::VariadicOperator(value) => format!("the symbol `{}`", value),
            Token::NonAssociativeOperator(value) => format!("the symbol `{}`", value),
            Token::Name(_) => self.add_article_prefix("name"),
            Token::Text(_) => self.add_article_prefix("piece of text"),
        }
    }

    pub fn render_syntax_kind(&self, kind: &crate::syntax::parse::SyntaxKind) -> String {
        use crate::syntax::parse::SyntaxKind;

        match kind {
            SyntaxKind::Number => self.add_article_prefix("number"),
            SyntaxKind::Name => self.add_article_prefix("name"),
            SyntaxKind::Text => self.add_article_prefix("piece of text"),
            SyntaxKind::TopLevel => String::from("the top level"),
            SyntaxKind::Attribute => self.add_article_prefix("attribute"),
            SyntaxKind::AttributeValue => self.add_article_prefix("attribute value"),
            SyntaxKind::Statement => self.add_article_prefix("statement"),
            SyntaxKind::LeftParenthesis => self.add_article_prefix("opening parenthesis (`(`)"),
            SyntaxKind::LeftBrace => self.add_article_prefix("opening brace (`{{`)"),
            SyntaxKind::Keyword(keyword) => format!("the word `{keyword}`"),
            SyntaxKind::ContextualKeyword(keyword) => format!("the word `{keyword}`"),
            SyntaxKind::Operator(operator) => format!("the symbol `{operator}`"),
            SyntaxKind::NonAssociativeOperator(operator) => format!("the symbol `{operator}`"),
            SyntaxKind::Instance => self.add_article_prefix("instance"),
            SyntaxKind::TypeParameter => self.add_article_prefix("type parameter"),
            SyntaxKind::Pattern => self.add_article_prefix("pattern"),
            SyntaxKind::WildcardPattern => self.add_article_prefix("wildcard pattern"),
            SyntaxKind::NumberPattern => self.add_article_prefix("number pattern"),
            SyntaxKind::TextPattern => self.add_article_prefix("text pattern"),
            SyntaxKind::VariantPattern => self.add_article_prefix("variant pattern"),
            SyntaxKind::DestructurePattern => self.add_article_prefix("destructure pattern"),
            SyntaxKind::TuplePattern => self.add_article_prefix("tuple pattern"),
            SyntaxKind::OrPattern => self.add_article_prefix("or pattern"),
            SyntaxKind::MutatePattern => self.add_article_prefix("mutate pattern"),
            SyntaxKind::AnnotatePattern => self.add_article_prefix("annotate pattern"),
            SyntaxKind::Expression => String::from("code"),
            SyntaxKind::Type => self.add_article_prefix("type"),
            SyntaxKind::PlaceholderType => self.add_article_prefix("placeholder type"),
            SyntaxKind::DeclaredType => self.add_article_prefix("declared type"),
            SyntaxKind::FunctionType => self.add_article_prefix("function type"),
            SyntaxKind::TupleType => self.add_article_prefix("tuple type"),
            SyntaxKind::BlockType => self.add_article_prefix("block type"),
            SyntaxKind::IntrinsicType => self.add_article_prefix("intrinsic type"),
            SyntaxKind::MessageType => self.add_article_prefix("message type"),
            SyntaxKind::EqualType => self.add_article_prefix("equal type"),
            SyntaxKind::TypeMember => self.add_article_prefix("type member"),
            SyntaxKind::FieldDeclaration => self.add_article_prefix("field declaration"),
            SyntaxKind::VariantDeclaration => self.add_article_prefix("variant declaration"),
            SyntaxKind::Arm => self.add_article_prefix("arm"),
            SyntaxKind::TypeFunction => self.add_article_prefix("type function"),
            SyntaxKind::TypeRepresentation => self.add_article_prefix("type representation"),
            SyntaxKind::SyntaxDeclaration => self.add_article_prefix("syntax declaration"),
            SyntaxKind::TypeDeclaration => self.add_article_prefix("type declaration"),
            SyntaxKind::TraitDeclaration => self.add_article_prefix("trait declaration"),
            SyntaxKind::InstanceDeclaration => self.add_article_prefix("instance declaration"),
            SyntaxKind::ConstantDeclaration => self.add_article_prefix("constant declaration"),
            SyntaxKind::LanguageDeclaration => self.add_article_prefix("language declaration"),
            SyntaxKind::Assignment => self.add_article_prefix("assignment"),
            SyntaxKind::AnnotateExpression => self.add_article_prefix("annotate expression"),
            SyntaxKind::NameExpression => self.add_article_prefix("name expression"),
            SyntaxKind::NumberExpression => self.add_article_prefix("number expression"),
            SyntaxKind::TextExpression => self.add_article_prefix("text expression"),
            SyntaxKind::DoExpression => self.add_article_prefix("do expression"),
            SyntaxKind::CallExpression => self.add_article_prefix("call expression"),
            SyntaxKind::ApplyExpression => self.add_article_prefix("apply expression"),
            SyntaxKind::BinaryOperatorExpression => {
                self.add_article_prefix("binary operator expression")
            }
            SyntaxKind::AsExpression => self.add_article_prefix("as expression"),
            SyntaxKind::IsExpression => self.add_article_prefix("is expression"),
            SyntaxKind::WhenExpression => self.add_article_prefix("when expression"),
            SyntaxKind::IntrinsicExpression => self.add_article_prefix("intrinsic expression"),
            SyntaxKind::TupleExpression => self.add_article_prefix("tuple expression"),
            SyntaxKind::CollectionExpression => self.add_article_prefix("collection expression"),
            SyntaxKind::StructureExpression => self.add_article_prefix("structure expression"),
            SyntaxKind::StructureField => self.add_article_prefix("structure field"),
            SyntaxKind::WhenBody => self.add_article_prefix("when body"),
            SyntaxKind::WhenArm => self.add_article_prefix("when arm"),
            SyntaxKind::BlockExpression => self.add_article_prefix("block expression"),
            SyntaxKind::FunctionExpression => self.add_article_prefix("function expression"),
            SyntaxKind::FunctionInputs => self.add_article_prefix("function inputs"),
            SyntaxKind::Nothing => self.add_article_prefix("nothing"),
        }
    }

    pub fn render_diagnostic_to_debug_string(&self, diagnostic: &RenderedDiagnostic) -> String {
        let line = diagnostic.location.start.line + 1;
        let column = diagnostic.location.start.column + 1;

        let severity = match diagnostic.severity {
            RenderedDiagnosticSeverity::Warning => "warning",
            RenderedDiagnosticSeverity::Error => "error",
        };

        let message = match &diagnostic.description {
            Some(description) => format!("{}: {}", diagnostic.message, description),
            None => diagnostic.message.clone(),
        };

        format!(
            "{}:{}:{}: {}: {}",
            diagnostic.location.path, line, column, severity, message
        )
    }

    pub fn render_documentation(
        &self,
        declaration: &WithInfo<AnyDeclaration>,
        full: bool,
    ) -> Option<RenderedDocumentation> {
        let rendered_declaration = self.render_declaration(declaration, full)?;

        let rendered_source_location = self.render_source_location(declaration)?;

        let line = rendered_source_location.start.line as usize;
        if line == 0 {
            return None;
        }

        let inner = self;
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
            AnyDeclarationKind::Syntax(declaration) => Some(&declaration.attributes),
            AnyDeclarationKind::Type(declaration) => Some(&declaration.attributes),
            AnyDeclarationKind::Trait(declaration) => Some(&declaration.attributes),
            AnyDeclarationKind::Constant(declaration) => Some(&declaration.attributes),
            AnyDeclarationKind::TypeParameter(_) | AnyDeclarationKind::Instance(_) => None,
        };

        let example = attributes.and_then(|attributes| {
            attributes.iter().find_map(|attribute| {
                if let crate::typecheck::Attribute::Valued { name, value } = &attribute.item {
                    if name.item == "example" {
                        if let crate::typecheck::AttributeValue::Text(text) = &value.item {
                            return Some(text.item.clone());
                        }
                    }
                }

                None
            })
        });

        Some(RenderedDocumentation {
            declaration: rendered_declaration,
            docs,
            example,
        })
    }

    pub fn render_highlight<T>(&self, value: &WithInfo<T>) -> Option<RenderedHighlight> {
        let declaration = self.get_declaration_from_info(&value.info, false)?;

        let attributes = match &declaration.item.kind {
            AnyDeclarationKind::Syntax(declaration) => Some(&declaration.attributes),
            AnyDeclarationKind::Type(declaration) => Some(&declaration.attributes),
            AnyDeclarationKind::Trait(declaration) => Some(&declaration.attributes),
            AnyDeclarationKind::Constant(declaration) => Some(&declaration.attributes),
            AnyDeclarationKind::TypeParameter(_) | AnyDeclarationKind::Instance(_) => None,
        };

        let mut options = RenderedHighlight::default();

        if let Some(attributes) = attributes {
            for attribute in attributes {
                if let crate::typecheck::Attribute::Valued { name, value } = &attribute.item {
                    if let crate::typecheck::AttributeValue::Text(text) = &value.item {
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

    fn compare_info(
        &self,
        left: &crate::syntax::Location,
        right: &crate::syntax::Location,
        between: bool,
    ) -> bool {
        left.path == right.path
            && if between {
                left.span.start >= right.span.start && left.span.end <= right.span.end
            } else {
                left.span == right.span
            }
    }

    fn name_for_path(&self, path: &crate::lower::Path) -> Option<String> {
        path.0
            .last()
            .and_then(|segment| segment.name())
            .map(ToString::to_string)
    }

    fn add_article_prefix(&self, s: &str) -> String {
        format!("{} {}", in_definite::get_a_or_an(s), s)
    }
}
