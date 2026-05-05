import * as vscode from "vscode";
import compiler, { IDERange, type CompileResult, type IDE } from "compiler";
import foundationLibrary from "../../library/dist/foundation.json";

const foundation = compiler.compile(foundationLibrary.files);
if (foundation != null) {
    compiler.registerLibrary("foundation", foundation);
}

const diagnosticCollection = vscode.languages.createDiagnosticCollection("wipple");

const log = vscode.window.createOutputChannel("wipple");

export const activate = (context: vscode.ExtensionContext) => {
    context.subscriptions.push(diagnosticCollection);

    vscode.workspace.textDocuments.forEach(update);
    vscode.workspace.onDidOpenTextDocument(update);
    vscode.workspace.onDidChangeTextDocument((e) => update(e.document));
    vscode.workspace.onDidCloseTextDocument(close);


    vscode.languages.registerDocumentSemanticTokensProvider(
        "wipple",
        { provideDocumentSemanticTokens },
        { tokenTypes, tokenModifiers: [] },
    );

    vscode.languages.registerHoverProvider("wipple", { provideHover });

    vscode.languages.registerDocumentHighlightProvider("wipple", { provideDocumentHighlights });

    vscode.languages.registerDefinitionProvider("wipple", { provideDefinition });

    vscode.languages.registerReferenceProvider("wipple", { provideReferences });

    vscode.languages.registerDocumentFormattingEditProvider("wipple", {
        provideDocumentFormattingEdits,
    });
};

const documents = new Map<vscode.TextDocument, IDE>();

const update = (document: vscode.TextDocument) => {
    if (document.languageId !== "wipple") return;

    documents.get(document)?.release();
    documents.delete(document);

    let result: CompileResult | null = null;
    let ide: IDE | null = null;
    try {
        result = compiler.compile(
            [{ path: document.uri.fsPath, code: document.getText() }],
            "foundation",
        );

        if (result == null) return;

        ide = new compiler.IDE(result);

        diagnosticCollection.set(
            document.uri,
            ide.diagnostics().map((diagnostic) => ({
                range: convertRange(diagnostic.range),
                message: diagnostic.message,
                severity: vscode.DiagnosticSeverity.Information,
                source: "wipple",
            })),
        );
    } finally {
        result?.release();
    }

    if (ide != null) {
        documents.set(document, ide);
    }
};

const close = (document: vscode.TextDocument) => {
    if (document.languageId !== "wipple") return;

    documents.get(document)?.release();
    documents.delete(document);

    diagnosticCollection.delete(document.uri);
};

const tokenTypes = ["type", "interface", "typeParameter", "function"];

const provideDocumentSemanticTokens = (
    document: vscode.TextDocument,
): vscode.ProviderResult<vscode.SemanticTokens> => {
    if (document.languageId !== "wipple") return;

    const ide = documents.get(document);
    if (ide == null) return;

    const builder = new vscode.SemanticTokensBuilder({ tokenTypes, tokenModifiers: [] });

    for (const token of ide.semanticTokens()) {
        builder.push(convertRange(token.range), token.type);
    }

    return builder.build();
};

const provideHover = (
    document: vscode.TextDocument,
    position: vscode.Position,
): vscode.ProviderResult<vscode.Hover> => {
    if (document.languageId !== "wipple") return;

    const ide = documents.get(document);
    if (ide == null) return;

    const hover = ide.hover(position.line + 1, position.character + 1);
    if (hover == null) return;

    return new vscode.Hover(
        hover.contents.map((item) => {
            const markedString = new vscode.MarkdownString();

            if (item.isCode) {
                markedString.appendCodeblock(item.value);
            } else {
                markedString.appendMarkdown(item.value);
            }

            return markedString;
        }),
        convertRange(hover.range),
    );
};

const provideDocumentHighlights = (
    document: vscode.TextDocument,
    position: vscode.Position,
): vscode.ProviderResult<vscode.DocumentHighlight[]> => {
    if (document.languageId !== "wipple") return;

    const ide = documents.get(document);
    if (ide == null) return;

    return ide
        .highlight(position.line + 1, position.character + 1)
        .map((range) => new vscode.DocumentHighlight(convertRange(range)));
};

const provideDefinition = (
    document: vscode.TextDocument,
    position: vscode.Position,
): vscode.ProviderResult<vscode.Location[]> => {
    if (document.languageId !== "wipple") return;

    const ide = documents.get(document);
    if (ide == null) return;

    const range = ide.definition(position.line + 1, position.character + 1);
    if (range == null) return;

    return [new vscode.Location(document.uri, convertRange(range))];
};

const provideReferences = (
    document: vscode.TextDocument,
    position: vscode.Position,
): vscode.ProviderResult<vscode.Location[]> => {
    if (document.languageId !== "wipple") return;

    const ide = documents.get(document);
    if (ide == null) return;

    const references = ide.references(position.line + 1, position.character + 1);

    return references.map((range) => new vscode.Location(document.uri, convertRange(range)));
};

const provideDocumentFormattingEdits = (
    document: vscode.TextDocument,
): vscode.ProviderResult<vscode.TextEdit[]> => {
    if (document.languageId !== "wipple") return;

    const formatted = compiler.format(document.getText());

    if (formatted == null) return [];

    return [
        vscode.TextEdit.replace(
            new vscode.Range(new vscode.Position(0, 0), new vscode.Position(document.lineCount, 0)),
            formatted,
        ),
    ];
};

const convertRange = (range: IDERange) =>
    new vscode.Range(
        new vscode.Position(range.start.line - 1, range.start.column - 1),
        new vscode.Position(range.end.line - 1, range.end.column - 1),
    );
