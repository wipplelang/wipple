import { readFileSync } from "node:fs";
import initWipple, * as wipple from "wipple";
import * as lsp from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";

const foundationBin = readFileSync(
    new URL("../../../library/dist/foundation.bin", import.meta.url),
);

const connection = lsp.createConnection(lsp.ProposedFeatures.all);
const documents = new lsp.TextDocuments(TextDocument);
let capabilities: lsp.ClientCapabilities;

connection.onInitialize(async (params) => {
    capabilities = params.capabilities;

    await initWipple({ module_or_path: readFileSync(wipple.modulePath) });
    wipple.register_library("foundation", foundationBin);

    return {
        capabilities: {
            textDocumentSync: lsp.TextDocumentSyncKind.Incremental,
            semanticTokensProvider: {
                legend: {
                    tokenTypes: tokenTypes,
                    tokenModifiers: [],
                },
                full: true,
            },
            hoverProvider: true,
            documentHighlightProvider: true,
            definitionProvider: true,
            referencesProvider: true,
            documentFormattingProvider: true,
            completionProvider: {
                resolveProvider: false,
            },
        },
    };
});

interface DocumentState {
    ide: wipple.Ide;
}

const documentStates = new Map<TextDocument, DocumentState>();

documents.onDidChangeContent(({ document }) => {
    if (document.languageId !== "wipple") return;

    documentStates.get(document)?.ide[Symbol.dispose]();
    documentStates.delete(document);

    using result = wipple.compile(
        [new wipple.File(document.uri, document.getText())],
        "foundation",
    );

    if (result == null) return;

    const ide = new wipple.Ide(result);

    connection.sendDiagnostics({
        uri: document.uri,
        diagnostics: ide.diagnostics().map(
            (diagnostic): lsp.Diagnostic => ({
                range: convertRange(diagnostic.range),
                message: capabilities.textDocument?.diagnostic?.markupMessageSupport
                    ? { kind: "markdown", value: diagnostic.message }
                    : diagnostic.message,
                severity: lsp.DiagnosticSeverity.Information,
                source: "wipple",
            }),
        ),
    });

    documentStates.set(document, { ide });
});

documents.onDidClose(({ document }) => {
    if (document.languageId !== "wipple") return;

    connection.sendDiagnostics({ uri: document.uri, diagnostics: [] });

    documentStates.get(document)?.ide[Symbol.dispose]();
    documentStates.delete(document);
});

const tokenTypes = ["type", "interface", "typeParameter", "function"];

connection.onRequest("textDocument/semanticTokens/full", (params) => {
    const document = documents.get(params.textDocument.uri);
    if (document == null || document.languageId !== "wipple") {
        return { data: [] };
    }

    const state = documentStates.get(document);
    if (state == null) {
        return { data: [] };
    }

    const builder = new lsp.SemanticTokensBuilder();
    for (const token of state.ide.semantic_tokens()) {
        const range = convertRange(token.range);
        if (range.start.line !== range.end.line) continue;

        builder.push(
            range.start.line,
            range.start.character,
            range.end.character - range.start.character,
            tokenTypes.indexOf(token.type),
            0,
        );
    }

    return builder.build();
});

connection.onHover((params) => {
    const document = documents.get(params.textDocument.uri);
    if (document == null || document.languageId !== "wipple") return;

    const state = documentStates.get(document);
    if (state == null) return;

    const hover = state.ide.hover(params.position.line + 1, params.position.character + 1);
    if (hover == null) return;

    return {
        contents: {
            kind: "markdown",
            value: hover.contents
                .map((item) => (item.is_code ? "```wipple\n" + item.value + "\n```" : item.value))
                .join("\n\n"),
        },
        range: convertRange(hover.range),
    };
});

connection.onDocumentHighlight((params) => {
    const document = documents.get(params.textDocument.uri);
    if (document == null || document.languageId !== "wipple") return;

    const state = documentStates.get(document);
    if (state == null) return;

    return state.ide
        .highlight(params.position.line + 1, params.position.character + 1)
        .map((range): lsp.DocumentHighlight => ({ range: convertRange(range) }));
});

connection.onDefinition((params) => {
    const document = documents.get(params.textDocument.uri);
    if (document == null || document.languageId !== "wipple") return;

    const state = documentStates.get(document);
    if (state == null) return;

    const definition = state.ide.definition(
        params.position.line + 1,
        params.position.character + 1,
    );
    if (definition == null) return;

    return {
        uri: document.uri,
        range: convertRange(definition),
    };
});

connection.onReferences((params) => {
    const document = documents.get(params.textDocument.uri);
    if (document == null || document.languageId !== "wipple") return;

    const state = documentStates.get(document);
    if (state == null) return;

    return state.ide
        .references(params.position.line + 1, params.position.character + 1)
        .map((range) => ({
            uri: document.uri,
            range: convertRange(range),
        }));
});

connection.onDocumentFormatting((params) => {
    const document = documents.get(params.textDocument.uri);
    if (document == null || document.languageId !== "wipple") return;

    const formatted = wipple.format(document.getText());
    if (formatted == null) return;

    return [
        {
            range: {
                start: { line: 0, character: 0 },
                end: { line: document.lineCount, character: 0 },
            },
            newText: formatted,
        },
    ];
});

connection.onCompletion((params) => {
    const document = documents.get(params.textDocument.uri);
    if (document == null || document.languageId !== "wipple") return;

    const state = documentStates.get(document);
    if (state == null) return;

    return state.ide
        .autocomplete(params.position.line + 1, params.position.character + 1)
        .map(({ name, type, definition, comments }): lsp.CompletionItem => {
            const kind = {
                variable: lsp.CompletionItemKind.Variable,
                function: lsp.CompletionItemKind.Function,
                class: lsp.CompletionItemKind.Class,
                interface: lsp.CompletionItemKind.Interface,
                typeParameter: lsp.CompletionItemKind.TypeParameter,
                constructor: lsp.CompletionItemKind.Constructor,
            }[type];

            return {
                label: name,
                kind,
                detail: definition,
                documentation: comments != null ? { kind: "markdown", value: comments } : undefined,
            };
        });
});

const convertRange = (range: wipple.IdeRange): lsp.Range => ({
    start: { line: range.start.line - 1, character: range.start.column - 1 },
    end: { line: range.end.line - 1, character: range.end.column - 1 },
});

documents.listen(connection);
connection.listen();
