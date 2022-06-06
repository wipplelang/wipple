#![allow(unreachable_code)]

mod macros;

use lsp_server::{Connection, ExtractError, Message, Notification, Response};
use lsp_types::{
    notification::{
        DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Notification as _,
        PublishDiagnostics,
    },
    request::HoverRequest,
    DiagnosticSeverity, Hover, HoverContents, HoverProviderCapability, InitializeParams,
    MarkupContent, MarkupKind, PublishDiagnosticsParams, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind,
};
use std::{borrow::Cow, cell::RefCell, collections::HashMap};

#[derive(Debug)]
struct Document {
    contents: String,
    line_index: Vec<usize>,
    info: Info,
}

#[derive(Debug)]
struct Info {
    diagnostics: Vec<wipple_compiler::diagnostics::Diagnostic>,
    annotations: Vec<Annotation>,
}

#[derive(Debug, Clone)]
struct Annotation {
    span: wipple_compiler::parse::Span,
    value: String,
}

pub fn start() -> anyhow::Result<()> {
    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        text_document_sync: Some(TextDocumentSyncCapability::Kind(
            TextDocumentSyncKind::INCREMENTAL,
        )),
        ..Default::default()
    })
    .unwrap();

    let initialization_params = connection.initialize(server_capabilities)?;
    main_loop(connection, initialization_params)?;
    io_threads.join()?;

    Ok(())
}

fn main_loop(connection: Connection, params: serde_json::Value) -> anyhow::Result<()> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();

    let mut documents = HashMap::<String, RefCell<Document>>::new();

    for msg in &connection.receiver {
        match msg {
            Message::Request(mut req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }

                match_req!(connection, req, {
                    HoverRequest(id, params) => {
                        let document = documents
                            .get(params.text_document_position_params.text_document.uri.as_str())
                            .unwrap();

                        let position = convert_position(
                            params.text_document_position_params.position,
                            &document.borrow().line_index
                        );

                        // Find the annotation with the smallest span covering the cursor

                        let mut annotations = document
                            .borrow()
                            .info
                            .annotations
                            .iter()
                            .filter(|annotation| {
                                (annotation.span.start..annotation.span.end)
                                    .contains(&position)
                            })
                            .cloned()
                            .collect::<Vec<_>>();

                        annotations.sort_by_key(|annotation| {
                            annotation.span.end - annotation.span.start
                        });

                        let annotation = (!annotations.is_empty()).then(|| {
                            annotations.swap_remove(0)
                        });

                        annotation.map(|annotation| Hover {
                            contents: HoverContents::Markup(MarkupContent {
                                kind: MarkupKind::Markdown,
                                value: format!("```wipple\n{}\n```", annotation.value),
                            }),
                            range: Some(lsp_types::Range {
                                start: convert_offset(annotation.span.start, &document.borrow().contents),
                                end: convert_offset(annotation.span.end, &document.borrow().contents),
                            }),
                        })
                    }
                });
            }
            Message::Notification(mut notif) => {
                match_notif!(notif, {
                    DidOpenTextDocument(params) => {
                        let line_index = build_line_index(&params.text_document.text);

                        let info = build(params.text_document.uri.as_str(), &documents);

                        documents.insert(params.text_document.uri.to_string(), RefCell::new(Document {
                            contents: params.text_document.text,
                            line_index,
                            info,
                        }));
                    }
                    DidCloseTextDocument(params) => {
                        documents.remove(params.text_document.uri.as_str());
                    }
                    DidChangeTextDocument(params) => {
                        let document = documents.get(params.text_document.uri.as_str()).unwrap();

                        {
                            let mut document = document.borrow_mut();

                            for change in params.content_changes {
                                let range = change.range.expect("no range provided");
                                let start = convert_position(range.start, &document.line_index);
                                let end = convert_position(range.end, &document.line_index);

                                document.contents.replace_range(start..end, &change.text);
                            }

                            document.line_index = build_line_index(&document.contents);
                        }

                        let info = build(params.text_document.uri.as_str(), &documents);

                        let diagnostics_notification = Notification::new(
                            String::from(PublishDiagnostics::METHOD),
                            PublishDiagnosticsParams {
                                uri: params.text_document.uri,
                                diagnostics: info
                                    .diagnostics
                                    .iter()
                                    .cloned()
                                    .flat_map(|mut diagnostic| {
                                        let first = diagnostic.notes.first_mut().unwrap();
                                        first.message = format!("{}\n{}", diagnostic.message, first.message);

                                        let severity = match diagnostic.level {
                                            wipple_compiler::diagnostics::DiagnosticLevel::Note => {
                                                DiagnosticSeverity::INFORMATION
                                            }
                                            wipple_compiler::diagnostics::DiagnosticLevel::Warning => {
                                                DiagnosticSeverity::WARNING
                                            }
                                            wipple_compiler::diagnostics::DiagnosticLevel::Error => {
                                                DiagnosticSeverity::ERROR
                                            }
                                        };

                                        diagnostic
                                            .notes
                                            .into_iter()
                                            .map(move |note| lsp_types::Diagnostic {
                                                range: lsp_types::Range {
                                                    start: convert_offset(note.span.start, &document.borrow().contents),
                                                    end: convert_offset(note.span.end, &document.borrow().contents),
                                                },
                                                severity: Some(match note.level {
                                                    wipple_compiler::diagnostics::NoteLevel::Primary => severity,
                                                    wipple_compiler::diagnostics::NoteLevel::Secondary => {
                                                        DiagnosticSeverity::INFORMATION
                                                    }
                                                }),
                                                source: Some(String::from("wipple")),
                                                message: note.message,
                                                ..Default::default()
                                            })
                                    })
                                    .collect(),
                                version: None,
                            },
                        );

                        document.borrow_mut().info = info;

                        connection.sender.send(Message::Notification(diagnostics_notification))?;
                    }
                });
            }
            Message::Response(_) => {}
        }
    }
    Ok(())
}

fn build(path: &str, documents: &HashMap<String, RefCell<Document>>) -> Info {
    struct Loader<'a> {
        documents: &'a HashMap<String, RefCell<Document>>,
    }

    impl<'a> wipple_compiler::Loader for Loader<'a> {
        type Error = anyhow::Error;

        fn load(&self, path: wipple_compiler::FilePath) -> Result<Cow<'static, str>, Self::Error> {
            match path {
                wipple_compiler::FilePath::Path(path) => Ok(Cow::Owned(
                    self.documents
                        .get(path.as_str())
                        .map(|document| document.borrow().contents.clone())
                        .unwrap_or_default(),
                )),
                wipple_compiler::FilePath::Virtual(_) => {
                    Err(anyhow::Error::msg("virtual paths are not supported"))
                }
                wipple_compiler::FilePath::Prelude => {
                    Ok(Cow::Borrowed(include_str!("../../../support/prelude.wpl")))
                }
                _ => unimplemented!(),
            }
        }
    }

    let loader = Loader { documents };
    let mut compiler = wipple_compiler::Compiler::new(loader, Default::default());

    let program = compiler.build(wipple_compiler::FilePath::Path(
        wipple_compiler::helpers::InternedString::new(path),
    ));

    let belongs_to_file = |span: wipple_compiler::parse::Span| {
        matches!(
            span.path,
            wipple_compiler::FilePath::Path(p) if p.as_str() == path,
        )
    };

    Info {
        diagnostics: compiler
            .finish()
            .diagnostics
            .into_iter()
            .filter(|diagnostic| {
                diagnostic
                    .notes
                    .iter()
                    .all(|note| belongs_to_file(note.span))
            })
            .collect(),
        annotations: program
            .map(|program| {
                use wipple_compiler::compile::typecheck::{format_type, ExpressionKind};

                let mut annotations = Vec::new();

                let declarations = program.declarations.clone();

                macro_rules! format_ty {
                    ($ty:expr) => {
                        format_type(
                            $ty,
                            |id| {
                                declarations
                                    .types
                                    .get(&id)
                                    .map(|decl| decl.name.as_str())
                                    .unwrap_or("<unknown>")
                                    .to_string()
                            },
                            |id| {
                                declarations
                                    .type_parameters
                                    .get(&id)
                                    .map(|decl| decl.name.as_str())
                                    .unwrap_or("<unknown>")
                                    .to_string()
                            },
                        )
                    };
                }

                macro_rules! add_annotation {
                    ($expr:expr) => {{
                        let expr = $expr;

                        if !belongs_to_file(expr.span) {
                            return;
                        }

                        let ty = format_ty!(expr.ty.clone());

                        let name = match expr.kind {
                            ExpressionKind::Variable(id) => {
                                declarations.variables.get(&id).map(|decl| decl.name)
                            }
                            ExpressionKind::Constant(id) => declarations
                                .monomorphized_constants
                                .get(&id)
                                .map(|((), decl)| decl.name),
                            _ => None,
                        };

                        annotations.push(Annotation {
                            span: expr.span,
                            value: match name {
                                Some(name) => format!("{name} :: {ty}"),
                                None => ty,
                            },
                        });
                    }};
                }

                program.traverse(|expr| add_annotation!(expr));

                for decl in declarations.types.values() {
                    if !belongs_to_file(decl.span) {
                        continue;
                    }

                    annotations.push(Annotation {
                        span: decl.span,
                        value: format!("{} : type", decl.name),
                    });
                }

                for decl in declarations.traits.values() {
                    if !belongs_to_file(decl.span) {
                        continue;
                    }

                    annotations.push(Annotation {
                        span: decl.span,
                        value: format!("{} : trait", decl.name),
                    });
                }

                for ((), decl) in &declarations.generic_constants {
                    if !belongs_to_file(decl.span) {
                        continue;
                    }

                    annotations.push(Annotation {
                        span: decl.span,
                        value: format!("{} :: {}", decl.name, format_ty!(decl.value.ty.clone())),
                    });

                    decl.value.traverse(|expr| add_annotation!(expr));
                }

                for decl in declarations.variables.values() {
                    if !belongs_to_file(decl.span) {
                        continue;
                    }

                    annotations.push(Annotation {
                        span: decl.span,
                        value: format!("{} :: {}", decl.name, format_ty!(decl.value.clone())),
                    });
                }

                annotations
            })
            .unwrap_or_default(),
    }
}

fn build_line_index(contents: &str) -> Vec<usize> {
    let lines = contents.split('\n').collect::<Vec<_>>();

    let mut line_index = Vec::new();
    let mut index = 0;
    for line in lines {
        line_index.push(index);
        index += line.len() + 1;
    }

    line_index
}

fn convert_position(position: lsp_types::Position, line_index: &[usize]) -> usize {
    line_index[position.line as usize] + position.character as usize
}

fn convert_offset(offset: usize, contents: &str) -> lsp_types::Position {
    let lookup = line_col::LineColLookup::new(contents);
    let (line, column) = lookup.get(offset);

    lsp_types::Position {
        line: (line - 1) as u32,
        character: (column - 1) as u32,
    }
}
