use futures::future::OptionFuture;
use line_index::{LineCol, LineIndex};
use serde::Deserialize;
use std::{
    collections::HashMap,
    fs, io,
    path::{Path, PathBuf},
    sync::Mutex,
};
use tower_lsp::{jsonrpc::Result, lsp_types::*, Client, LanguageServer, LspService, Server};

pub async fn start() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        config: Mutex::new(Config {
            root_dir: std::env::current_dir().expect("failed to retreive working directory"),
            render: wipple_render::Render::new(),
            dependencies: Default::default(),
            source_file_paths: Default::default(),
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
    dependencies: Vec<wipple_driver::Interface>,
    source_file_paths: Vec<PathBuf>,
    diagnostics: Vec<wipple_driver::util::WithInfo<wipple_driver::Info, wipple_driver::Diagnostic>>,
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

        #[derive(Default, Deserialize)]
        #[serde(rename_all = "camelCase")]
        struct Configuration {
            source_dependencies: Vec<PathBuf>,
            source_files: Vec<PathBuf>,
        }

        if let Some(project) = params
            .initialization_options
            .as_ref()
            .and_then(|options| options.get("project").cloned())
            .and_then(|option| serde_json::from_value::<Configuration>(option).ok())
        {
            config.dependencies = project
                .source_dependencies
                .into_iter()
                .filter_map(read_binary)
                .collect();

            config.source_file_paths = project.source_files;
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
                document_formatting_provider: Some(OneOf::Left(true)),
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

        let config = self.config.lock().unwrap();

        let diagnostics = config
            .diagnostics
            .iter()
            .flat_map(|diagnostic| config.render.render_diagnostic(diagnostic))
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
                    message: format!("{}: {:?}", diagnostic.template.id, diagnostic.template.data),
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
        let sources = OptionFuture::from(params.text_document.uri.to_file_path().ok().and_then(
            |active_file_path| {
                let config = self.config.lock().unwrap();

                let active_file = config.files.get(&active_file_path)?;
                let active_code = active_file.text.clone();
                let active_line_index = active_file.line_index.clone();

                Some(async move {
                    (
                        self.get_sources(&active_file_path, &active_code).await,
                        active_line_index,
                    )
                })
            },
        ))
        .await;

        let config = self.config.lock().unwrap();

        let actions = params
            .context
            .diagnostics
            .into_iter()
            .filter_map(|context_diagnostic| {
                let index = context_diagnostic.data.as_ref()?.as_u64()? as usize;
                let diagnostic = config.diagnostics.get(index)?;
                Some((context_diagnostic, diagnostic))
            })
            .flat_map(|(context_diagnostic, diagnostic)| {
                let mut actions = Vec::new();

                if let Some((sources, line_index)) = sources.clone() {
                    let dependencies = config.dependencies.clone();

                    if let Some((fix, new_code)) =
                        wipple_driver::fix_file(diagnostic.clone(), sources, dependencies)
                    {
                        let message = config.render.render_fix(&fix);

                        let start = line_index.line_col(line_index::TextSize::new(0));
                        let end = line_index.line_col(line_index.len());

                        actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                            title: message,
                            kind: Some(CodeActionKind::QUICKFIX),
                            diagnostics: Some(vec![context_diagnostic]),
                            edit: Some(WorkspaceEdit {
                                changes: Some(HashMap::from([(
                                    params.text_document.uri.clone(),
                                    vec![TextEdit {
                                        range: Range {
                                            start: Position {
                                                line: start.line,
                                                character: start.col,
                                            },
                                            end: Position {
                                                line: end.line,
                                                character: end.col,
                                            },
                                        },
                                        new_text: new_code,
                                    }],
                                )])),
                                document_changes: None,
                                change_annotations: None,
                            }),
                            ..Default::default()
                        }));
                    }
                }

                actions
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

        let symbol_range = wipple_driver::util::find_word_boundary(&file.text, position as usize);
        let symbol = &file.text[symbol_range.clone()];

        let render = self.config.lock().unwrap().render.clone();

        let path = wipple_driver::util::get_visible_path(&path);

        if let Some((declaration, declaration_range)) = render
            .get_path_at_cursor(&path, position)
            .and_then(|declaration_path| {
                Some((
                    render.get_declaration_from_path(&declaration_path.item)?,
                    range_from_info(&declaration_path.info, &file),
                ))
            })
            .or_else(|| {
                render
                    .get_declaration_for_syntax(symbol)
                    .map(|declaration| {
                        let start = file.line_index.line_col((symbol_range.start as u32).into());
                        let end = file.line_index.line_col((symbol_range.end as u32).into());

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

                        (declaration, range)
                    })
            })
        {
            range = Some(declaration_range);

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

                        content.push(format!("```wipple\ninstance {code} -- from bound\n```"));
                    }
                }
            }

            if let Some(rendered_documentation) = render.render_documentation(&declaration) {
                content.push(rendered_documentation.docs);

                if let Some(example) = rendered_documentation.example {
                    content.push(format!("[Example]({example})"));
                }
            }
        }

        if let Some(expression) = render.get_expression_at_cursor(&path, position) {
            range = Some(range_from_info(&expression.info, &file));

            let r#type = expression.map(|expression| expression.r#type);

            let code = render.render_type(
                &r#type,
                true,
                wipple_render::DescribeOptions::NoDescribe,
                false,
            );

            content.push(format!("```wipple\n{code}\n```"));
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
                    wipple_render::RenderedSuggestionKind::Syntax => {
                        Some(CompletionItemKind::KEYWORD)
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

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let path = match params.text_document.uri.to_file_path() {
            Ok(path) => path,
            Err(_) => return Ok(None),
        };

        let file = match self.config.lock().unwrap().files.get(&path) {
            Some(file) => file.clone(),
            None => return Ok(None),
        };

        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: file.line_index.len().into(),
                character: 0,
            },
        };

        let code = wipple_driver::format(&file.text) + "\n";

        Ok(Some(vec![TextEdit {
            range,
            new_text: code,
        }]))
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

impl Backend {
    async fn get_sources(&self, active_file: &Path, active_code: &str) -> Vec<wipple_driver::File> {
        let active_file = match active_file.canonicalize().ok() {
            Some(path) => path,
            None => return Vec::new(),
        };

        self.config
            .lock()
            .unwrap()
            .source_file_paths
            .iter()
            .filter_map(|path| {
                let code = if *path == active_file {
                    // Ensure we get the latest changes even if the file
                    // isn't saved
                    active_code.to_string()
                } else {
                    fs::read_to_string(path).ok()?
                };

                Some(wipple_driver::File {
                    path: path.to_string_lossy().to_string(),
                    visible_path: wipple_driver::util::get_visible_path(path),
                    code,
                })
            })
            .collect()
    }

    async fn compile_all(
        &self,
        active_file: &Path,
        active_code: &str,
    ) -> Vec<wipple_driver::util::WithInfo<wipple_driver::Info, wipple_driver::Diagnostic>> {
        let sources = self.get_sources(active_file, active_code).await;

        let (dependencies, result, render) = {
            let config = self.config.lock().unwrap();
            let dependencies = config.dependencies.clone();

            let result = wipple_driver::compile(sources, dependencies.clone());

            let render = config.render.clone();

            (dependencies, result, render)
        };

        render.update(
            dependencies.into_iter().chain([result.interface]).collect(),
            Some(result.library),
            Some(result.ide),
        );

        result.diagnostics
    }
}

fn read_binary<T: serde::de::DeserializeOwned>(path: impl AsRef<Path>) -> Option<T> {
    wipple_driver::util::read_binary(io::BufReader::new(fs::File::open(path).ok()?)).ok()
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
