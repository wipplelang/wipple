use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};
use tokio::sync::RwLock;
use tower_lsp_server::{Client, LanguageServer, LspService, Server, jsonrpc::Result, ls_types::*};
use wipple::{
    database::{Db, NodeRef, RenderConfig, Span},
    driver,
    feedback::{FeedbackWriter, collect_feedback},
    queries::{self, QueryCtx},
    syntax::{format, parse},
    typecheck::Type,
};

#[tokio::main]
pub async fn lsp(lib: Option<PathBuf>) -> anyhow::Result<()> {
    let mut db = Db::new();

    db.render_with(RenderConfig::new(|db, value, f| {
        write!(f, "`")?;
        value.write(f, db)?;
        write!(f, "`")?;
        Ok(())
    }));

    if let Some(path) = lib {
        let layer = driver::read_layer(&mut db, path)?;
        driver::compile(&mut db, &layer.files);
    }

    let (service, socket) = LspService::new(|client| Backend {
        client,
        base: db,
        state: Default::default(),
    });

    Server::new(tokio::io::stdin(), tokio::io::stdout(), socket)
        .serve(service)
        .await;

    Ok(())
}

#[derive(Debug)]
struct Backend {
    client: Client,
    base: Db,
    state: RwLock<State>,
}

#[derive(Debug, Default)]
struct State {
    files: HashMap<Uri, (String, Db)>,
}

impl Backend {
    async fn update(&self, uri: &Uri) {
        let mut state = self.state.write().await;

        let (source, db_entry) = state.files.get_mut(uri).expect("no source for document");

        let mut db = self.base.clone();

        let file = parse(&mut db, uri.as_str(), source);
        driver::compile(&mut db, &[file]); // TODO: support multiple files

        let diagnostics = get_feedback(&db, node_filter(&db, uri));

        *db_entry = db;

        drop(state);

        self.client
            .send_notification::<notification::PublishDiagnostics>(PublishDiagnosticsParams {
                uri: uri.clone(),
                diagnostics,
                version: None,
            })
            .await;
    }
}

const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::TYPE,
    SemanticTokenType::INTERFACE,
    SemanticTokenType::TYPE_PARAMETER,
    SemanticTokenType::FUNCTION,
];

impl LanguageServer for Backend {
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            legend: SemanticTokensLegend {
                                token_types: TOKEN_TYPES.to_vec(),
                                ..Default::default()
                            },
                            ..Default::default()
                        },
                    ),
                ),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                document_highlight_provider: Some(OneOf::Left(true)),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.state.write().await.files.insert(
            params.text_document.uri.clone(),
            (params.text_document.text, Db::new()),
        );

        self.update(&params.text_document.uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.state.write().await.files.insert(
            params.text_document.uri.clone(),
            (
                params.content_changes.into_iter().next().unwrap().text,
                Db::new(),
            ),
        );

        self.update(&params.text_document.uri).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.state
            .write()
            .await
            .files
            .remove(&params.text_document.uri);
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let state = self.state.read().await;

        let Some((source, db)) = state.files.get(&params.text_document.uri) else {
            return Ok(None);
        };

        let tokens = get_semantic_tokens(db, source, &params.text_document.uri);

        Ok(Some(SemanticTokensResult::Tokens(tokens)))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let state = self.state.read().await;

        let uri = &params.text_document_position_params.text_document.uri;
        let Some((_, db)) = state.files.get(uri) else {
            return Ok(None);
        };

        Ok(get_hover(
            db,
            uri,
            params.text_document_position_params.position,
        ))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let state = self.state.read().await;

        let uri = &params.text_document_position_params.text_document.uri;

        let Some((_, db)) = state.files.get(uri) else {
            return Ok(None);
        };

        let Some(node) =
            get_node_at_position(db, uri, params.text_document_position_params.position)
        else {
            return Ok(None);
        };

        let ctx = QueryCtx { db, node };

        let mut nodes = Vec::new();
        queries::definitions(&ctx, &mut |definitions| {
            nodes.extend(definitions);
        });

        Ok(Some(GotoDefinitionResponse::Array(get_locations(
            db, &nodes, uri,
        ))))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let state = self.state.read().await;

        let uri = &params.text_document_position.text_document.uri;

        let Some((_, db)) = state.files.get(uri) else {
            return Ok(None);
        };

        let Some(node) = get_node_at_position(db, uri, params.text_document_position.position)
        else {
            return Ok(None);
        };

        let ctx = QueryCtx { db, node };

        let mut nodes = Vec::new();
        queries::references(&ctx, &mut |reference| {
            nodes.push(reference);
        });

        Ok(Some(get_locations(db, &nodes, uri)))
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> Result<Option<Vec<DocumentHighlight>>> {
        let state = self.state.read().await;

        let uri = &params.text_document_position_params.text_document.uri;

        let Some((_, db)) = state.files.get(uri) else {
            return Ok(None);
        };

        Ok(Some(get_related(
            db,
            uri,
            params.text_document_position_params.position,
        )))
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let state = self.state.read().await;

        let Some((source, _)) = state.files.get(&params.text_document.uri) else {
            return Ok(None);
        };

        let Some(formatted) = format(source) else {
            return Ok(None);
        };

        Ok(Some(vec![TextEdit {
            range: Range {
                start: Position::new(0, 0),
                end: Position::new(source.lines().count() as u32 + 1, 0),
            },
            new_text: format!("{formatted}\n"),
        }]))
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

fn node_filter(db: &Db, uri: &Uri) -> impl Fn(&NodeRef) -> bool {
    |node| db.span(node).path == uri.as_str()
}

fn convert_span(span: Span) -> Range {
    Range {
        start: Position {
            line: span.start.line as u32 - 1,
            character: span.start.column as u32 - 1,
        },
        end: Position {
            line: span.end.line as u32 - 1,
            character: span.end.column as u32 - 1,
        },
    }
}

fn get_feedback(db: &Db, filter: impl Fn(&NodeRef) -> bool) -> Vec<Diagnostic> {
    collect_feedback(db, |item| filter(&item.location.0))
        .into_iter()
        .map(|item| {
            let mut message = String::new();
            (item.write)(db, &mut message);

            Diagnostic {
                range: convert_span(db.span(&item.location.0)),
                severity: Some(DiagnosticSeverity::INFORMATION),
                message,
                source: Some(String::from("wipple")),
                ..Default::default()
            }
        })
        .collect::<Vec<_>>()
}

fn get_semantic_tokens(db: &Db, source: &str, uri: &Uri) -> SemanticTokens {
    let filter = node_filter(db, uri);

    let mut tokens = HashMap::new();
    for node in db.iter_nodes().filter(filter) {
        let span = db.span(&node);

        // Don't highlight across whitespace
        if span.start.line != span.end.line || span.source.contains(char::is_whitespace) {
            continue;
        }

        let ctx = QueryCtx { db, node };

        queries::highlight_type(&ctx, &mut |()| {
            tokens.insert(span.clone(), SemanticTokenType::TYPE);
        });

        queries::highlight_trait(&ctx, &mut |()| {
            tokens.insert(span.clone(), SemanticTokenType::INTERFACE);
        });

        queries::highlight_type_parameter(&ctx, &mut |()| {
            tokens.insert(span.clone(), SemanticTokenType::TYPE_PARAMETER);
        });

        queries::highlight_function(&ctx, &mut |()| {
            tokens.insert(span.clone(), SemanticTokenType::FUNCTION);
        });
    }

    let mut tokens = Vec::from_iter(tokens);
    tokens.sort_by_key(|(span, _)| span.start.index);

    // Adapted from https://github.com/IWANABETHATGUY/tower-lsp-boilerplate
    let mut pre_line = 0;
    let mut pre_start = 0;
    let data = tokens
        .iter()
        .map(|(span, token)| {
            let line = span.start.line as u32 - 1;

            let line_index = source[..span.start.index]
                .char_indices()
                .rev()
                .find(|&(_, c)| c == '\n')
                .map(|(index, _)| index + 1)
                .unwrap_or_default() as u32;

            let start = span.start.index as u32 - line_index;

            let delta_line = line - pre_line;
            let delta_start = if delta_line == 0 {
                start - pre_start
            } else {
                start
            };

            pre_line = line;
            pre_start = start;

            SemanticToken {
                delta_line,
                delta_start,
                length: (span.end.index - span.start.index) as u32,
                token_type: TOKEN_TYPES.iter().position(|t| t == token).unwrap() as u32,
                token_modifiers_bitset: 0,
            }
        })
        .collect::<Vec<_>>();

    SemanticTokens {
        result_id: None,
        data,
    }
}

fn get_hover(db: &Db, uri: &Uri, position: Position) -> Option<Hover> {
    let node_at_position = get_node_at_position(db, uri, position)?;

    let mut contents = Vec::new();
    let ctx = QueryCtx {
        db,
        node: node_at_position.clone(),
    };

    queries::has_type(&ctx, &mut |ty| {
        contents.push(MarkedString::LanguageString(LanguageString {
            language: String::from("wipple"),
            value: format!(
                "{} :: {}",
                db.span(&node_at_position).as_node_source(),
                Type::Constructed(ty).to_string(db, true)
            ),
        }));
    });

    queries::resolved_bound(&ctx, &mut |(_, resolved)| {
        let value = db.span(&resolved.instance_node).as_definition_source();

        let value = if resolved.from_bound {
            format!("instance ({value})")
        } else {
            value
        };

        contents.push(MarkedString::LanguageString(LanguageString {
            language: String::from("wipple"),
            value,
        }))
    });

    queries::comments(&ctx, &mut |data| {
        let mut docs = String::new();
        FeedbackWriter::new(db, &mut docs).write_comments(&data);

        let docs = docs.trim();
        if !docs.is_empty() {
            contents.push(MarkedString::String(docs.to_string()));
        }
    });

    if contents.is_empty() {
        return None;
    }

    Some(Hover {
        contents: HoverContents::Array(contents),
        range: Some(convert_span(db.span(&node_at_position))),
    })
}

fn get_related(db: &Db, uri: &Uri, position: Position) -> Vec<DocumentHighlight> {
    let filter = node_filter(db, uri);

    let Some(node_at_position) = get_node_at_position(db, uri, position) else {
        return Vec::new();
    };

    let mut highlights = vec![DocumentHighlight {
        range: convert_span(db.span(&node_at_position)),
        kind: None,
    }];

    let ctx = QueryCtx {
        db,
        node: node_at_position,
    };

    queries::related(&ctx, &mut |related| {
        if !filter(&related) {
            return;
        }

        highlights.push(DocumentHighlight {
            range: convert_span(db.span(&related)),
            kind: None,
        });
    });

    highlights
}

fn get_locations(db: &Db, nodes: &[NodeRef], open_uri: &Uri) -> Vec<Location> {
    let mut seen = HashSet::new();
    let mut locations = Vec::new();
    for node in nodes {
        let span = db.span(node);

        if !seen.insert(span.clone()) {
            continue;
        }

        let uri = if span.path == open_uri.as_str() {
            open_uri.clone()
        } else {
            match Uri::from_file_path(&span.path) {
                Some(uri) => uri,
                None => continue,
            }
        };

        locations.push(Location {
            uri,
            range: convert_span(span),
        });
    }

    locations
}

fn get_node_at_position(db: &Db, uri: &Uri, position: Position) -> Option<NodeRef> {
    let filter = node_filter(db, uri);

    let mut matches = Vec::new();
    for node in db.iter_nodes().filter(filter) {
        let range = convert_span(db.span(&node));

        if range.start.line == position.line
            && range.start.character <= position.character
            && range.end.line == position.line
            && range.end.character >= position.character
        {
            let length = range.end.character - range.start.character;
            matches.push((length, node));
        }
    }

    matches.sort_by_key(|&(length, _)| length);
    matches.into_iter().next().map(|(_, node)| node)
}
