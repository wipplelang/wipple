use line_index::{LineCol, LineIndex};
use serde::Deserialize;
use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
    sync::Mutex,
};
use tower_lsp::{jsonrpc::Result, lsp_types::*, Client, LanguageServer, LspService, Server};
use wipple_driver::util::lazy_static::lazy_static;

#[derive(Deserialize)]
struct BuiltinHelp {
    docs: String,
    example: Option<String>,
}

lazy_static! {
    static ref BUILTINS_HELP: HashMap<String, BuiltinHelp> =
        serde_json::from_str(include_str!("../../library/help/builtins.json")).unwrap();
}

pub async fn start() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        config: Mutex::new(Config {
            root_dir: std::env::current_dir().expect("failed to retreive working directory"),
            render: wipple_render::Render::new(),
            dependencies: Default::default(),
            libraries: Default::default(),
            diagnostics: Default::default(),
            files: Default::default(),
        }),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}

struct Backend {
    client: Client,
    config: Mutex<Config>,
}

struct Config {
    root_dir: PathBuf,
    render: wipple_render::Render,
    dependencies: Option<wipple_driver::Interface>,
    libraries: Vec<wipple_driver::Library>,
    diagnostics: Vec<wipple_render::RenderedDiagnostic>,
    files: HashMap<PathBuf, File>,
}

#[derive(Clone)]
struct File {
    text: String,
    line_index: LineIndex,
}

impl File {
    fn new(text: String) -> Self {
        let line_index = LineIndex::new(&text);
        File { text, line_index }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        let mut config = self.config.lock().unwrap();

        if let Some(root_uri) = params
            .workspace_folders
            .as_ref()
            .and_then(|folders| folders.first())
            .and_then(|folder| folder.uri.to_file_path().ok())
        {
            config.root_dir = root_uri;
        }

        if let Some(interface) = params
            .initialization_options
            .as_ref()
            .and_then(|options| options.get("interface"))
            .and_then(|path| read_to_json(path.as_str()?, &config.root_dir))
        {
            config.dependencies = Some(interface);
        };

        if let Some(libraries) = params
            .initialization_options
            .as_ref()
            .and_then(|options| options.get("libraries"))
            .and_then(|option| option.as_array())
            .and_then(|paths| {
                paths
                    .iter()
                    .map(|path| read_to_json(path.as_str()?, &config.root_dir))
                    .collect()
            })
        {
            config.libraries = libraries;
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                diagnostic_provider: Some(DiagnosticServerCapabilities::RegistrationOptions(
                    DiagnosticRegistrationOptions {
                        text_document_registration_options: TextDocumentRegistrationOptions {
                            document_selector: Some(vec![DocumentFilter {
                                language: Some(String::from("wipple")),
                                pattern: None,
                                scheme: None,
                            }]),
                        },
                        diagnostic_options: DiagnosticOptions {
                            inter_file_dependencies: true,
                            workspace_diagnostics: false,
                            ..Default::default()
                        },
                        ..Default::default()
                    },
                )),
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions::default()),
                definition_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "connection initialized")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let path = match params.text_document.uri.to_file_path() {
            Ok(path) => path,
            Err(_) => return,
        };

        let text = params.text_document.text;

        self.config
            .lock()
            .unwrap()
            .files
            .insert(path.clone(), File::new(text.clone()));

        let diagnostics = self.compile_all(&path, &text).await;
        self.config.lock().unwrap().diagnostics = diagnostics;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let path = match params.text_document.uri.to_file_path() {
            Ok(path) => path,
            Err(_) => return,
        };

        let text = match params
            .content_changes
            .first()
            .map(|change| change.text.as_str())
        {
            Some(text) => text,
            None => return,
        };

        self.config
            .lock()
            .unwrap()
            .files
            .insert(path.clone(), File::new(text.to_string()));

        let diagnostics = self.compile_all(&path, text).await;
        self.config.lock().unwrap().diagnostics = diagnostics;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let path = match params.text_document.uri.to_file_path() {
            Ok(path) => path,
            Err(_) => return,
        };

        self.config.lock().unwrap().files.remove(&path);
    }

    async fn diagnostic(
        &self,
        params: DocumentDiagnosticParams,
    ) -> Result<DocumentDiagnosticReportResult> {
        let path = match params
            .text_document
            .uri
            .to_file_path()
            .ok()
            .and_then(|path| path.canonicalize().ok())
        {
            Some(path) => path,
            None => {
                return Ok(DocumentDiagnosticReportResult::Report(
                    DocumentDiagnosticReport::Full(Default::default()),
                ));
            }
        };

        let diagnostics = self.config.lock().unwrap().diagnostics.clone();
        let diagnostics = diagnostics
            .into_iter()
            .filter(|diagnostic| path.to_str() == Some(&diagnostic.location.path))
            .enumerate()
            .map(|(index, diagnostic)| {
                let severity = match diagnostic.severity {
                    wipple_render::RenderedDiagnosticSeverity::Error => DiagnosticSeverity::ERROR,
                    wipple_render::RenderedDiagnosticSeverity::Warning => {
                        DiagnosticSeverity::WARNING
                    }
                };

                Diagnostic {
                    severity: Some(severity),
                    range: Range {
                        start: Position {
                            line: diagnostic.location.start.line,
                            character: diagnostic.location.start.column,
                        },
                        end: Position {
                            line: diagnostic.location.end.line,
                            character: diagnostic.location.end.column,
                        },
                    },
                    message: diagnostic.message,
                    source: Some(String::from("wipple")),
                    data: Some((index as u64).into()),
                    ..Default::default()
                }
            })
            .collect();

        Ok(DocumentDiagnosticReportResult::Report(
            DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
                full_document_diagnostic_report: FullDocumentDiagnosticReport {
                    items: diagnostics,
                    ..Default::default()
                },
                ..Default::default()
            }),
        ))
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let config = self.config.lock().unwrap();

        let actions = params
            .context
            .diagnostics
            .into_iter()
            .filter_map(|diagnostic| {
                let index = diagnostic.data.as_ref()?.as_u64()? as usize;
                let rendered_diagnostic = config.diagnostics.get(index)?;
                let fix = rendered_diagnostic.fix.as_ref()?.clone();

                let before_edit = fix.before.map(|text| TextEdit {
                    range: Range {
                        start: Position {
                            line: rendered_diagnostic.location.start.line,
                            character: rendered_diagnostic.location.start.column,
                        },
                        end: Position {
                            line: rendered_diagnostic.location.start.line,
                            character: rendered_diagnostic.location.start.column,
                        },
                    },
                    new_text: text,
                });

                let edit = fix.replacement.map(|text| TextEdit {
                    range: Range {
                        start: Position {
                            line: rendered_diagnostic.location.start.line,
                            character: rendered_diagnostic.location.start.column,
                        },
                        end: Position {
                            line: rendered_diagnostic.location.end.line,
                            character: rendered_diagnostic.location.end.column,
                        },
                    },
                    new_text: text,
                });

                let after_edit = fix.after.map(|text| TextEdit {
                    range: Range {
                        start: Position {
                            line: rendered_diagnostic.location.end.line,
                            character: rendered_diagnostic.location.end.column,
                        },
                        end: Position {
                            line: rendered_diagnostic.location.end.line,
                            character: rendered_diagnostic.location.end.column,
                        },
                    },
                    new_text: text,
                });

                Some(CodeActionOrCommand::CodeAction(CodeAction {
                    title: fix.message,
                    kind: Some(CodeActionKind::QUICKFIX),
                    diagnostics: Some(vec![diagnostic]),
                    edit: Some(WorkspaceEdit {
                        changes: Some(HashMap::from([(
                            params.text_document.uri.clone(),
                            [before_edit, edit, after_edit]
                                .into_iter()
                                .flatten()
                                .collect(),
                        )])),
                        document_changes: None,
                        change_annotations: None,
                    }),
                    ..Default::default()
                }))
            })
            .collect::<Vec<_>>();

        if actions.is_empty() {
            return Ok(None);
        }

        Ok(Some(actions))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let path = match params
            .text_document_position_params
            .text_document
            .uri
            .to_file_path()
        {
            Ok(path) => path,
            Err(_) => return Ok(None),
        };

        let file = match self.config.lock().unwrap().files.get(&path) {
            Some(file) => file.clone(),
            None => return Ok(None),
        };

        let position = match file.line_index.offset(LineCol {
            line: params.text_document_position_params.position.line,
            col: params.text_document_position_params.position.character,
        }) {
            Some(position) => u32::from(position),
            None => return Ok(None),
        };

        let mut range = None;
        let mut content = Vec::new();

        let symbol_range = find_word_boundary(&file.text, position as usize);
        let symbol = &file.text[symbol_range.clone()];
        if let Some(help) = BUILTINS_HELP.get(symbol) {
            let start = file.line_index.line_col((symbol_range.start as u32).into());
            let end = file.line_index.line_col((symbol_range.end as u32).into());

            range = Some(Range {
                start: Position {
                    line: start.line,
                    character: start.col,
                },
                end: Position {
                    line: end.line,
                    character: end.col,
                },
            });

            content.push(format!("```wipple\n{symbol}\n```"));
            content.push(help.docs.clone());

            if let Some(example) = &help.example {
                content.push(format!("[Example]({example})"));
            }
        } else {
            let render = self.config.lock().unwrap().render.clone();

            let path = wipple_driver::util::get_visible_path(&path);
            if let Some(declaration_path) = render.get_path_at_cursor(&path, position) {
                if let Some(declaration) = render.get_declaration_from_path(&declaration_path.item)
                {
                    range = Some(range_from_info(&declaration_path.info, &file));

                    if let Some(code) = render.render_declaration(&declaration) {
                        content.push(format!("```wipple\n{code}\n```"));
                    }

                    // If the expression is a trait, also render the resolved instance
                    if let Some(instance) = instance_at_cursor(&render, &path, position) {
                        match instance.item {
                            Ok(declaration) => {
                                let declaration = wipple_driver::util::WithInfo {
                                    info: instance.info,
                                    item: declaration,
                                };

                                if let Some(code) = render.render_declaration(&declaration) {
                                    content.push(format!("```wipple\n{code}\n```"));
                                }
                            }
                            Err(bound) => {
                                let bound = wipple_driver::util::WithInfo {
                                    info: instance.info,
                                    item: bound,
                                };

                                let code = render.render_instance(&bound, false);

                                content
                                    .push(format!("```wipple\ninstance {code} -- from bound\n```"));
                            }
                        }
                    }

                    if let Some(rendered_documentation) = render.render_documentation(&declaration)
                    {
                        content.push(rendered_documentation.docs);

                        if let Some(example) = rendered_documentation.example {
                            content.push(format!("[Example]({example})"));
                        }
                    }
                }
            } else if let Some(expression) = render.get_expression_at_cursor(&path, position) {
                range = Some(range_from_info(&expression.info, &file));

                let r#type = expression.map(|expression| expression.r#type);

                let code = render.render_type(&r#type, true, false, false);
                content.push(format!("```wipple\n{code}\n```"));
            }
        }

        if content.is_empty() {
            return Ok(None);
        }

        Ok(Some(Hover {
            range,
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: content.join("\n\n"),
            }),
        }))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let path = match params
            .text_document_position
            .text_document
            .uri
            .to_file_path()
        {
            Ok(path) => path,
            Err(_) => return Ok(None),
        };

        let file = match self.config.lock().unwrap().files.get(&path) {
            Some(file) => file.clone(),
            None => return Ok(None),
        };

        let position = match file.line_index.offset(LineCol {
            line: params.text_document_position.position.line,
            col: params.text_document_position.position.character,
        }) {
            Some(position) => u32::from(position),
            None => return Ok(None),
        };

        let path = wipple_driver::util::get_visible_path(&path);

        let render = self.config.lock().unwrap().render.clone();
        let suggestions = render.render_suggestions_at_cursor(&path, position);

        let completions = suggestions
            .into_iter()
            .map(|suggestion| CompletionItem {
                kind: match suggestion.kind {
                    wipple_render::RenderedSuggestionKind::Type => Some(CompletionItemKind::CLASS),
                    wipple_render::RenderedSuggestionKind::Trait => {
                        Some(CompletionItemKind::INTERFACE)
                    }
                    wipple_render::RenderedSuggestionKind::TypeParameter => {
                        Some(CompletionItemKind::TYPE_PARAMETER)
                    }
                    wipple_render::RenderedSuggestionKind::Constant => {
                        Some(CompletionItemKind::CONSTANT)
                    }
                    wipple_render::RenderedSuggestionKind::Variable => {
                        Some(CompletionItemKind::VARIABLE)
                    }
                    wipple_render::RenderedSuggestionKind::Keyword => {
                        Some(CompletionItemKind::KEYWORD)
                    }
                    wipple_render::RenderedSuggestionKind::Operator => {
                        Some(CompletionItemKind::OPERATOR)
                    }
                },
                label: suggestion.name,
                detail: suggestion.code,
                documentation: suggestion.docs.map(|docs| {
                    Documentation::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: docs.docs,
                    })
                }),
                ..Default::default()
            })
            .collect::<Vec<_>>();

        Ok(Some(CompletionResponse::Array(completions)))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let path = match params
            .text_document_position_params
            .text_document
            .uri
            .to_file_path()
        {
            Ok(path) => path,
            Err(_) => return Ok(None),
        };

        let file = match self.config.lock().unwrap().files.get(&path) {
            Some(file) => file.clone(),
            None => return Ok(None),
        };

        let position = match file.line_index.offset(LineCol {
            line: params.text_document_position_params.position.line,
            col: params.text_document_position_params.position.character,
        }) {
            Some(position) => u32::from(position),
            None => return Ok(None),
        };

        let path = wipple_driver::util::get_visible_path(&path);

        let render = self.config.lock().unwrap().render.clone();

        let declaration_path = match render.get_path_at_cursor(&path, position) {
            Some(declaration) => declaration,
            None => return Ok(None),
        };

        let declaration = match render.get_declaration_from_path(&declaration_path.item) {
            Some(declaration) => declaration,
            None => return Ok(None),
        };

        let location = match location_from_info(&declaration.info) {
            Some(location) => location,
            None => return Ok(None),
        };

        let mut locations = vec![location];

        // If the expression is a trait, also show the resolved instance
        if let Some(instance) = instance_at_cursor(&render, &path, position) {
            if let Some(location) = location_from_info(&instance.info) {
                locations.push(location);
            }
        }

        Ok(Some(GotoDefinitionResponse::Array(locations)))
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

impl Backend {
    async fn compile_all(
        &self,
        active_file: &Path,
        active_code: &str,
    ) -> Vec<wipple_render::RenderedDiagnostic> {
        let active_file = match active_file.canonicalize().ok() {
            Some(path) => path,
            None => return Vec::new(),
        };

        let active_dir = match active_file.parent() {
            Some(dir) => dir,
            None => return Vec::new(),
        };

        let sources = match fs::read_dir(active_dir).ok().map(|entries| {
            entries
                .filter_map(|entry| {
                    let entry = entry.ok()?;
                    let path = entry.path();

                    if !path.is_file() || path.extension()?.to_str() != Some("wipple") {
                        return None;
                    }

                    let code = if path == active_file {
                        // Ensure we get the latest changes even if the file
                        // isn't saved
                        active_code.to_string()
                    } else {
                        fs::read_to_string(&path).ok()?
                    };

                    Some(wipple_driver::File {
                        path: path.to_string_lossy().to_string(),
                        visible_path: wipple_driver::util::get_visible_path(&path),
                        code,
                    })
                })
                .collect::<Vec<_>>()
        }) {
            Some(paths) => paths,
            None => return Vec::new(),
        };

        let (result, render, libraries) = {
            let config = self.config.lock().unwrap();
            let dependencies = config.dependencies.clone();

            let result = wipple_driver::compile(sources, dependencies);

            let render = config.render.clone();
            let libraries = config.libraries.clone();

            (result, render, libraries)
        };

        render.update(
            result.interface,
            [result.library].into_iter().chain(libraries).collect(),
            Some(result.ide),
        );

        result
            .diagnostics
            .iter()
            .flat_map(|diagnostic| render.render_diagnostic(diagnostic))
            .collect()
    }
}

fn read_to_json<T: for<'de> serde::Deserialize<'de>>(
    path: impl AsRef<Path>,
    root: &Path,
) -> Option<T> {
    serde_json::from_str(&fs::read_to_string(root.join(path)).ok()?).ok()
}

fn find_word_boundary(text: &str, position: usize) -> std::ops::Range<usize> {
    unicode_segmentation::UnicodeSegmentation::grapheme_indices(text, true)
        .find(|(index, _)| *index >= position)
        .map(|(start, str)| {
            let end = start + str.len();
            start..end
        })
        .unwrap_or(position..position)
}

fn range_from_info(info: &wipple_driver::Info, file: &File) -> Range {
    let start = file.line_index.line_col(info.location.span.start.into());
    let end = file.line_index.line_col(info.location.span.end.into());

    Range {
        start: Position {
            line: start.line,
            character: start.col,
        },
        end: Position {
            line: end.line,
            character: end.col,
        },
    }
}

fn location_from_info(info: &wipple_driver::Info) -> Option<Location> {
    let uri = Url::from_file_path(info.location.path.as_ref()).ok()?;

    let file = File::new(fs::read_to_string(info.location.path.as_ref()).ok()?);
    let start = file.line_index.line_col(info.location.span.start.into());
    let end = file.line_index.line_col(info.location.span.end.into());

    let range = Range {
        start: Position {
            line: start.line,
            character: start.col,
        },
        end: Position {
            line: end.line,
            character: end.col,
        },
    };

    Some(Location { uri, range })
}

fn instance_at_cursor(
    render: &wipple_render::Render,
    path: &str,
    position: u32,
) -> Option<
    wipple_driver::util::WithInfo<
        wipple_driver::Info,
        std::result::Result<
            wipple_render::AnyDeclaration,
            wipple_driver::typecheck::Instance<wipple_driver::Driver>,
        >,
    >,
> {
    if let Some(expression) = render.get_expression_at_cursor(path, position) {
        if let wipple_driver::typecheck::TypedExpressionKind::Constant { path, bounds, .. } =
            expression.item.kind
        {
            if let Some(declaration) = render.get_declaration_from_path(&path) {
                if let wipple_render::AnyDeclarationKind::Trait(_) = declaration.item.kind {
                    // The constructor for a trait mentions the trait in its first
                    // bound (traits themselves may not directly have bounds; if
                    // that changes, this will need to be updated)
                    if let Some(instance) = bounds.into_iter().next() {
                        match instance.item {
                            Ok(path) => {
                                if let Some(declaration) = render.get_declaration_from_path(&path) {
                                    return Some(declaration.map(Ok));
                                }
                            }
                            Err(bound) => {
                                return Some(wipple_driver::util::WithInfo {
                                    info: instance.info,
                                    item: Err(bound),
                                });
                            }
                        }
                    }
                }
            }
        }
    }

    None
}
