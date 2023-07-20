use parking_lot::{MappedRwLockReadGuard, RwLock, RwLockReadGuard};
use std::{collections::HashMap, sync::Arc};
use tower_lsp::{jsonrpc, lsp_types::*, Client, LanguageServer, LspService, Server};
use wipple_default_loader::{Fetcher, Loader};
use wipple_frontend::{
    analysis::{
        lower::AnyDeclaration,
        typecheck::{
            format::{format_type, Format, TypeFunctionFormat},
            SyntaxDecl, TraitDecl, Type, TypeDecl, TypeDeclKind,
        },
        Expression, ExpressionKind, Program, Span,
    },
    diagnostics::DiagnosticLevel,
    helpers::InternedString,
    Compiler, FilePath,
};

pub async fn run() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let loader = Loader::new(
        None,
        Some({
            wipple_frontend::FilePath::Path(
                #[cfg(debug_assertions)]
                wipple_frontend::helpers::InternedString::new(concat!(
                    env!("CARGO_WORKSPACE_DIR"),
                    "std/std.wpl"
                )),
                #[cfg(not(debug_assertions))]
                wipple_frontend::helpers::InternedString::new(wipple_default_loader::STD_URL),
            )
        }),
    )
    .with_fetcher(
        Fetcher::new()
            .with_default_path_handler()
            .with_default_url_handler(),
    );

    let (service, socket) = LspService::new(|client| Backend {
        client,
        compiler: Compiler::new(loader),
        documents: Default::default(),
    });

    Server::new(stdin, stdout, socket).serve(service).await;
}

#[derive(Clone)]
struct Backend {
    client: Client,
    compiler: Compiler,
    documents: Arc<RwLock<HashMap<FilePath, Document>>>,
}

struct Document {
    path: FilePath,
    source: String,
    program: Program,
}

impl Document {
    fn line_col_lookup(&self) -> LineColLookup {
        LineColLookup::new(&self.source)
    }

    fn offset_lookup(&self) -> OffsetLookup {
        OffsetLookup::new(&self.source)
    }
}

struct LineColLookup<'a> {
    len: usize,
    lookup: line_col::LineColLookup<'a>,
}

impl<'a> LineColLookup<'a> {
    fn new(src: &'a str) -> Self {
        LineColLookup {
            len: src.len(),
            lookup: line_col::LineColLookup::new(src),
        }
    }

    fn get(&self, pos: usize) -> Option<(usize, usize)> {
        (pos <= self.len).then(|| self.lookup.get(pos))
    }
}

struct OffsetLookup {
    lines: Vec<usize>,
}

impl OffsetLookup {
    fn new(s: &str) -> Self {
        OffsetLookup {
            lines: s.lines().map(|line| line.len() + 1).collect(),
        }
    }

    fn get(&self, line: usize, col: usize) -> usize {
        self.lines[0..line].iter().sum::<usize>() + col
    }
}

const LANGUAGE_ID: &str = "wipple";
const SEMANTIC_TOKENS_LEGEND: &[SemanticTokenType] = &[
    SemanticTokenType::FUNCTION,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::MACRO,
    SemanticTokenType::TYPE,
    SemanticTokenType::TYPE_PARAMETER,
    SemanticTokenType::INTERFACE,
    SemanticTokenType::VARIABLE,
];

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![DocumentFilter {
                                        language: Some(String::from(LANGUAGE_ID)),
                                        scheme: Some(String::from("file")),
                                        pattern: None,
                                    }]),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: SEMANTIC_TOKENS_LEGEND.to_vec(),
                                    token_modifiers: Vec::new(),
                                },
                                range: Some(false),
                                full: Some(SemanticTokensFullOptions::Delta { delta: Some(false) }),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions::default()),
                document_formatting_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Wipple language server initialized")
            .await;

        std::env::set_var("RUST_BACKTRACE", "1");
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.on_change(params.text_document).await;
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentItem {
            language_id: String::from(LANGUAGE_ID),
            uri: params.text_document.uri,
            text: std::mem::take(&mut params.content_changes[0].text),
            version: params.text_document.version,
        })
        .await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let path = self.file_path_from(&params.text_document.uri);
        self.documents.write().remove(&path);
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> jsonrpc::Result<Option<SemanticTokensResult>> {
        let document = self.document_from(&params.text_document.uri)?;

        let mut semantic_tokens = Vec::new();

        macro_rules! insert_semantic_tokens {
            ($kind:ident, $token:expr) => {
                for decl in document.program.declarations.$kind.values() {
                    if decl.span.first().path == document.path {
                        semantic_tokens.push((decl.span, $token(decl)));
                    }

                    for &span in &decl.uses {
                        if span.first().path == document.path {
                            semantic_tokens.push((span, $token(decl)));
                        }
                    }
                }
            };
        }

        insert_semantic_tokens!(types, |_| SemanticTokenType::TYPE);
        insert_semantic_tokens!(traits, |_| SemanticTokenType::INTERFACE);
        insert_semantic_tokens!(constants, |_| SemanticTokenType::VARIABLE);
        insert_semantic_tokens!(syntaxes, |decl: &SyntaxDecl| {
            if decl.keyword {
                SemanticTokenType::KEYWORD
            } else if decl.operator {
                SemanticTokenType::OPERATOR
            } else {
                SemanticTokenType::MACRO
            }
        });
        insert_semantic_tokens!(builtin_types, |_| SemanticTokenType::TYPE);
        insert_semantic_tokens!(type_parameters, |_| SemanticTokenType::TYPE_PARAMETER);
        insert_semantic_tokens!(variables, |_| SemanticTokenType::VARIABLE);

        let mut traverse_semantic_tokens = |expr: &Expression| {
            if expr.span.first().path != document.path {
                return;
            }

            if matches!(
                expr.kind,
                ExpressionKind::Variable(_) | ExpressionKind::Constant(_)
            ) && matches!(expr.ty, Type::Function(_, _))
            {
                semantic_tokens.push((expr.span, SemanticTokenType::FUNCTION));
            }
        };

        for decl in document.program.declarations.constants.values() {
            if let Some(expr) = &decl.body {
                expr.traverse(&mut traverse_semantic_tokens);
            }
        }

        for instances in document.program.declarations.instances.values() {
            for decl in instances.values() {
                if let Some(expr) = &decl.body {
                    expr.traverse(&mut traverse_semantic_tokens);
                }
            }
        }

        for item in document.program.items.values() {
            let item = item.read();
            let (constant, expr) = &*item;

            if constant.is_some() {
                // Skip monomorphized constant types
                continue;
            }

            expr.traverse(&mut traverse_semantic_tokens);
        }

        semantic_tokens.reverse();
        semantic_tokens.sort_by_key(|(span, _)| span.first().primary_start());
        semantic_tokens.dedup_by_key(|(span, _)| *span);

        let mut pre_line = 0;
        let mut pre_start = 0;
        let line_col_lookup = document.line_col_lookup();

        let data = semantic_tokens
            .into_iter()
            .filter_map(|(span, semantic_token_type)| {
                let (line, start_col) = line_col_lookup.get(
                    span.first()
                        .caller_start()
                        .unwrap_or_else(|| span.first().primary_start()),
                )?;

                let (_, end_col) = line_col_lookup.get(
                    span.first()
                        .caller_end()
                        .unwrap_or_else(|| span.first().primary_end())
                        - 1,
                )?;

                let line = line - 1;
                let start_col = start_col - 1;
                let end_col = end_col;

                let length = (end_col - start_col) as u32;
                let delta_line = (line - pre_line) as u32;
                let delta_start = if delta_line == 0 {
                    (start_col - pre_start) as u32
                } else {
                    start_col as u32
                };

                let result = Some(SemanticToken {
                    delta_line,
                    delta_start,
                    length,
                    token_type: SEMANTIC_TOKENS_LEGEND
                        .iter()
                        .position(|t| *t == semantic_token_type)
                        .expect("invalid semantic token type")
                        as u32,
                    token_modifiers_bitset: 0,
                });

                pre_line = line;
                pre_start = start_col;

                result
            })
            .collect::<Vec<_>>();

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data,
        })))
    }

    async fn hover(&self, params: HoverParams) -> jsonrpc::Result<Option<Hover>> {
        let document =
            self.document_from(&params.text_document_position_params.text_document.uri)?;
        let line_col_lookup = document.line_col_lookup();
        let offset_lookup = document.offset_lookup();

        let position = params.text_document_position_params.position;
        let position = offset_lookup.get(position.line as usize, position.character as usize);
        let hover_span = Span::new(document.path, position..position);

        let within_hover = |span: Span| hover_span.is_subspan_of(span);

        let range_from = |span: Span| {
            let (start_line, start_col) = line_col_lookup.get(span.primary_start())?;
            let (end_line, end_col) = line_col_lookup.get(span.primary_end())?;

            Some(Range::new(
                Position::new(start_line as u32 - 1, start_col as u32 - 1),
                Position::new(end_line as u32 - 1, end_col as u32 - 1),
            ))
        };

        fn code_segment(code: impl ToString) -> MarkedString {
            MarkedString::LanguageString(LanguageString {
                language: String::from(LANGUAGE_ID),
                value: code.to_string(),
            })
        }

        let format_type = |ty: Type, format: Format| {
            macro_rules! getter {
                ($kind:ident, $f:expr) => {
                    |id| $f(document.program.declarations.$kind.get(&id).unwrap().name)
                };
            }

            format_type(
                ty,
                getter!(types, |name: InternedString| name.to_string()),
                getter!(traits, |name: InternedString| name.to_string()),
                getter!(type_parameters, |name: Option<_>| {
                    name.as_ref().map(ToString::to_string)
                }),
                format,
            )
        };

        let mut hovers = Vec::new();

        for item in document.program.items.values() {
            let item = item.read();
            let (constant, expr) = &*item;

            if constant.is_some() {
                // Skip monomorphized constant types
                continue;
            }

            expr.traverse(|expr| {
                // Don't show type of entire file
                if let Some(entrypoint) = document.program.entrypoint {
                    if let Some(item) = document.program.items.get(&entrypoint) {
                        let item = item.read();
                        let (_, item) = &*item;

                        if expr.span == item.span {
                            return;
                        }
                    }
                }

                if matches!(
                    expr.kind,
                    ExpressionKind::Variable(_) | ExpressionKind::Constant(_)
                ) {
                    return;
                }

                if !within_hover(expr.span.first()) {
                    return;
                }

                if let Some(range) = range_from(expr.span.first()) {
                    let contents = code_segment(format_type(expr.ty.clone(), Format::default()));

                    hovers.push((
                        expr.span,
                        Hover {
                            range: Some(range),
                            contents: HoverContents::Scalar(contents),
                        },
                    ));
                }
            })
        }

        macro_rules! type_decls {
            ($kind:ident $(($opt:tt))?, $str:literal $(, $help:expr)?) => {
                for decl in document.program.declarations.$kind.values() {
                    for span in std::iter::once(decl.span).chain(decl.uses.iter().copied()) {
                        if !within_hover(span.first()) {
                            continue;
                        }

                        let range = match range_from(span.first()) {
                            Some(range) => range,
                            None => continue,
                        };

                        #[allow(unused_mut)]
                        let mut contents = vec![code_segment(format!("{} : {}", decl.name, $str))];

                        $(
                            contents.extend(
                                $help(decl)
                                    .into_iter()
                                    .map(|line| MarkedString::String(line.to_string()))
                                    .collect::<Vec<_>>()
                            );
                        )?

                        hovers.push((
                            span,
                            Hover {
                                range: Some(range),
                                contents: HoverContents::Array(contents),
                            },
                        ));
                    }
                }
            };
        }

        type_decls!(types, "type", |decl: &TypeDecl| {
            decl.attributes.decl_attributes.help.clone()
        });

        type_decls!(traits, "trait", |decl: &TraitDecl| {
            decl.attributes.decl_attributes.help.clone()
        });

        for decl in document.program.declarations.constants.values() {
            for span in std::iter::once(decl.span).chain(decl.uses.iter().copied()) {
                if !within_hover(span.first()) {
                    continue;
                }

                let range = match range_from(span.first()) {
                    Some(range) => range,
                    None => continue,
                };

                let format = Format {
                    type_function: TypeFunctionFormat::Arrow(&decl.bounds),
                    ..Default::default()
                };

                let mut contents = vec![code_segment(format!(
                    "{} :: {}",
                    decl.name,
                    format_type(
                        decl.reduced_ty.clone().unwrap_or_else(|| decl.ty.clone()),
                        format
                    )
                ))];

                contents.extend(
                    decl.attributes
                        .decl_attributes
                        .help
                        .iter()
                        .map(|line| MarkedString::String(line.to_string()))
                        .collect::<Vec<_>>(),
                );

                hovers.push((
                    span,
                    Hover {
                        range: Some(range),
                        contents: HoverContents::Array(contents),
                    },
                ));
            }
        }

        for decl in document.program.declarations.variables.values() {
            for span in std::iter::once(decl.span).chain(decl.uses.iter().copied()) {
                if !within_hover(span.first()) {
                    continue;
                }

                let range = match range_from(span.first()) {
                    Some(range) => range,
                    None => continue,
                };

                let name = match decl.name {
                    Some(name) => name,
                    None => continue,
                };

                let contents = code_segment(format!(
                    "{} :: {}",
                    name,
                    format_type(decl.ty.clone(), Format::default())
                ));

                hovers.push((
                    span,
                    Hover {
                        range: Some(range),
                        contents: HoverContents::Scalar(contents),
                    },
                ));
            }
        }

        Ok(hovers
            .into_iter()
            .min_by_key(|(span, _)| span.first().primary_end() - span.first().primary_start())
            .map(|(_, hover)| hover))
    }

    async fn completion(
        &self,
        params: CompletionParams,
    ) -> jsonrpc::Result<Option<CompletionResponse>> {
        let document = self.document_from(&params.text_document_position.text_document.uri)?;
        let offset_lookup = document.offset_lookup();

        let position = params.text_document_position.position;
        let position = offset_lookup.get(position.line as usize, position.character as usize);
        let cursor_span = Span::new(document.path, position..position);

        let _within_cursor = |span: Span| cursor_span.is_subspan_of(span);

        let format_type = |ty: Type, format: Format| {
            macro_rules! getter {
                ($kind:ident, $f:expr) => {
                    |id| $f(document.program.declarations.$kind.get(&id).unwrap().name)
                };
            }

            format_type(
                ty,
                getter!(types, |name: InternedString| name.to_string()),
                getter!(traits, |name: InternedString| name.to_string()),
                getter!(type_parameters, |name: Option<_>| {
                    name.as_ref().map(ToString::to_string)
                }),
                format,
            )
        };

        let mut items = Vec::new();

        let _add = |scope: &HashMap<InternedString, AnyDeclaration>| {
            for (name, value) in scope {
                let kind;
                let mut help = Vec::new();
                let mut ty = None;
                let mut format = Format::default();

                match value {
                    AnyDeclaration::Type(id) => {
                        kind = Some(
                            match document.program.declarations.types.get(id).unwrap().kind {
                                TypeDeclKind::Marker
                                | TypeDeclKind::Structure { .. }
                                | TypeDeclKind::Alias(_) => CompletionItemKind::STRUCT,
                                TypeDeclKind::Enumeration { .. } => CompletionItemKind::ENUM,
                            },
                        );

                        help = document
                            .program
                            .declarations
                            .types
                            .get(id)
                            .unwrap()
                            .attributes
                            .decl_attributes
                            .help
                            .clone();
                    }
                    AnyDeclaration::BuiltinType(id) => {
                        kind = Some(CompletionItemKind::STRUCT);

                        help = document
                            .program
                            .declarations
                            .builtin_types
                            .get(id)
                            .unwrap()
                            .attributes
                            .help
                            .clone();
                    }
                    AnyDeclaration::Trait(id) => {
                        kind = Some(CompletionItemKind::INTERFACE);

                        help = document
                            .program
                            .declarations
                            .traits
                            .get(id)
                            .unwrap()
                            .attributes
                            .decl_attributes
                            .help
                            .clone();
                    }
                    AnyDeclaration::TypeParameter(_) => {
                        kind = Some(CompletionItemKind::TYPE_PARAMETER);
                    }
                    AnyDeclaration::Constant(id, _) => {
                        kind = Some(CompletionItemKind::CONSTANT);

                        let decl = document.program.declarations.constants.get(id).unwrap();

                        help = decl.attributes.decl_attributes.help.clone();

                        ty = Some(decl.ty.clone());

                        format.type_function = TypeFunctionFormat::Arrow(&decl.bounds);
                    }
                    AnyDeclaration::Variable(id) => {
                        kind = Some(CompletionItemKind::VARIABLE);

                        ty = document
                            .program
                            .declarations
                            .variables
                            .get(id)
                            .map(|decl| decl.ty.clone());
                    }
                }

                if let Some(kind) = kind {
                    let item = CompletionItem {
                        label: name.to_string(),
                        kind: Some(kind),
                        detail: ty.map(|ty| format_type(ty, format)),
                        documentation: Some(Documentation::MarkupContent(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: help
                                .into_iter()
                                .map(|line| line.to_string())
                                .collect::<Vec<_>>()
                                .join("\n"),
                        })),
                        ..Default::default()
                    };

                    items.push(item);
                }
            }
        };

        // for (span, scope) in &document.program.scopes {
        //     if !within_cursor(*span) {
        //         continue;
        //     }

        //     add(scope);
        // }

        // add(&document.program.exported);

        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn formatting(
        &self,
        params: DocumentFormattingParams,
    ) -> jsonrpc::Result<Option<Vec<TextEdit>>> {
        let document = self.document_from(&params.text_document.uri)?;

        let formatted = match wipple_syntax::parse::format(&document.source) {
            Some(code) => code,
            None => return Ok(None),
        };

        let line_col_lookup = document.line_col_lookup();

        let (start_line, start_col) = match line_col_lookup.get(0) {
            Some(range) => range,
            None => return Ok(None),
        };

        let (end_line, end_col) = match line_col_lookup.get(document.source.len()) {
            Some(range) => range,
            None => return Ok(None),
        };

        let range = Range::new(
            Position::new(start_line as u32 - 1, start_col as u32 - 1),
            Position::new(end_line as u32 - 1, end_col as u32 - 1),
        );

        Ok(Some(vec![TextEdit {
            range,
            new_text: formatted,
        }]))
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }
}

impl Backend {
    fn file_path_from(&self, uri: &Url) -> FilePath {
        FilePath::Path(self.raw_file_path_from(uri))
    }

    fn raw_file_path_from(&self, uri: &Url) -> InternedString {
        InternedString::new(uri.path())
    }

    fn document_from(&self, uri: &Url) -> jsonrpc::Result<MappedRwLockReadGuard<Document>> {
        RwLockReadGuard::try_map(self.documents.read(), |documents| {
            documents.get(&self.file_path_from(uri))
        })
        .map_err(|_| jsonrpc::Error::internal_error())
    }

    async fn on_change(&self, text_document: TextDocumentItem) {
        let backend = self.clone();

        let analysis = backend.clone().analyze(&text_document);

        let (program, diagnostics) = analysis.await;

        let path = backend.file_path_from(&text_document.uri);

        let document = Document {
            path,
            source: text_document.text,
            program,
        };

        backend
            .update_diagnostics(text_document.uri, &document, diagnostics)
            .await;

        backend.documents.write().insert(path, document);
    }

    async fn analyze(
        self,
        document: &TextDocumentItem,
    ) -> (Program, Vec<wipple_frontend::diagnostics::Diagnostic>) {
        let path = self.file_path_from(&document.uri);

        self.compiler.loader.virtual_paths().lock().insert(
            self.raw_file_path_from(&document.uri),
            Arc::from(document.text.as_str()),
        );

        let (program, diagnostics) = self.compiler.analyze_with(path, &Default::default()).await;

        (program, diagnostics.diagnostics)
    }

    async fn update_diagnostics(
        &self,
        uri: Url,
        document: &Document,
        diagnostics: Vec<wipple_frontend::diagnostics::Diagnostic>,
    ) {
        let diagnostics = {
            let line_col_lookup = document.line_col_lookup();
            let range_from = |span: Span, use_caller: bool| {
                let range = use_caller
                    .then(|| span.caller_range())
                    .flatten()
                    .unwrap_or_else(|| span.primary_range());

                let (start_line, start_col) = line_col_lookup.get(range.start)?;
                let (end_line, end_col) = line_col_lookup.get(range.end)?;

                Some(Range::new(
                    Position::new(start_line as u32 - 1, start_col as u32 - 1),
                    Position::new(end_line as u32 - 1, end_col as u32 - 1),
                ))
            };

            let mut result = Vec::new();

            for diagnostic in diagnostics {
                let mut notes = diagnostic.notes.into_iter();

                let primary_note = notes.next().unwrap();
                if primary_note.span.first().path != document.path {
                    continue;
                }

                let severity = match diagnostic.level {
                    DiagnosticLevel::Warning => DiagnosticSeverity::WARNING,
                    DiagnosticLevel::Error => DiagnosticSeverity::ERROR,
                };

                let range = match range_from(
                    primary_note.span.first(),
                    primary_note.use_caller_if_available,
                ) {
                    Some(range) => range,
                    None => continue,
                };

                result.push(Diagnostic {
                    range,
                    severity: Some(severity),
                    source: Some(String::from("wipple")),
                    message: format!("{}\n{}", diagnostic.message, primary_note.message),
                    ..Default::default()
                });

                for note in notes {
                    if note.span.first().path != document.path {
                        continue;
                    }

                    let range = match range_from(note.span.first(), note.use_caller_if_available) {
                        Some(range) => range,
                        None => continue,
                    };

                    result.push(Diagnostic {
                        range,
                        severity: Some(DiagnosticSeverity::INFORMATION),
                        source: Some(String::from("wipple")),
                        message: note.message,
                        ..Default::default()
                    });
                }
            }

            result
        };

        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }
}
