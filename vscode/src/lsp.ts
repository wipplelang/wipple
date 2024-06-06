import { TextDocument } from "vscode-languageserver-textdocument";
import {
    Diagnostic,
    DiagnosticSeverity,
    DocumentDiagnosticReportKind,
    ProposedFeatures,
    TextDocumentSyncKind,
    TextDocuments,
    createConnection,
    CompletionItem,
    CompletionItemKind,
} from "vscode-languageserver/node";
import { URI } from "vscode-uri";
import * as fs from "fs/promises";
import * as path from "path";
import * as compiler from "wipple-compiler";
import { Render, RenderedDiagnostic } from "wipple-render";

const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

const render = new Render();
let diagnostics: RenderedDiagnostic[] = [];

const connection = createConnection(ProposedFeatures.all);

let rootDir = ".";
let dependencies: compiler.Interface | null = null;
let libraries: compiler.linker_UnlinkedLibrary[] = [];

connection.onInitialize(async (params) => {
    const rootUri = params.workspaceFolders?.[0]?.uri;
    if (rootUri) {
        rootDir = URI.parse(rootUri).fsPath;
    }

    if (params.initializationOptions?.interface) {
        dependencies = JSON.parse(
            await fs.readFile(
                path.resolve(rootDir, params.initializationOptions.interface),
                "utf8",
            ),
        );
    }

    if (params.initializationOptions?.libraries) {
        libraries = await Promise.all(
            params.initializationOptions.libraries.map(async (libraryPath: string) =>
                JSON.parse(await fs.readFile(path.resolve(rootDir, libraryPath), "utf8")),
            ),
        );
    }

    return {
        capabilities: {
            textDocumentSync: TextDocumentSyncKind.Full,
            diagnosticProvider: {
                documentSelector: [{ language: "wipple" }],
                interFileDependencies: true,
                workspaceDiagnostics: false,
            },
            hoverProvider: true,
            completionProvider: {},
            definitionProvider: true,
        },
    };
});

connection.onInitialized(() => {
    console.log("Connection initialized");
});

const getVisiblePath = (uri: string) => `${path.basename(path.dirname(uri))}/${path.basename(uri)}`;

documents.onDidChangeContent(async (change) => {
    const uri = URI.parse(change.document.uri);
    if (uri.scheme !== "file") {
        return;
    }

    diagnostics = (await compileAll(uri.fsPath, change.document.getText())).flatMap(
        (diagnostic) => {
            const rendered = render.renderDiagnostic(diagnostic);
            return rendered ? [rendered] : [];
        },
    );

    connection.languages.diagnostics.refresh();
});

connection.languages.diagnostics.on(async (params) => {
    const uri = URI.parse(params.textDocument.uri);
    if (uri.scheme !== "file") {
        return {
            kind: DocumentDiagnosticReportKind.Full,
            items: [],
        };
    }

    return {
        kind: DocumentDiagnosticReportKind.Full,
        items: diagnostics
            .filter(
                (diagnostic) =>
                    path.normalize(diagnostic.location.path) === path.normalize(uri.fsPath),
            )
            .map((diagnostic): Diagnostic => {
                let severity: DiagnosticSeverity;
                switch (diagnostic.severity) {
                    case "warning":
                        severity = DiagnosticSeverity.Warning;
                        break;
                    case "error":
                        severity = DiagnosticSeverity.Error;
                        break;
                }

                return {
                    severity,
                    range: {
                        start: {
                            line: diagnostic.location.start.line,
                            character: diagnostic.location.start.column,
                        },
                        end: {
                            line: diagnostic.location.end.line,
                            character: diagnostic.location.end.column,
                        },
                    },
                    message: diagnostic.message,
                    source: "wipple",
                };
            }),
    };
});

connection.onHover(async (params) => {
    const uri = URI.parse(params.textDocument.uri);
    if (uri.scheme !== "file") {
        return null;
    }

    const document = documents.get(params.textDocument.uri);
    if (!document) {
        return null;
    }

    const position = document.offsetAt(params.position);

    const declarationPath = render.getPathAtCursor(getVisiblePath(uri.fsPath), position);
    if (!declarationPath) {
        return null;
    }

    const declaration = render.getDeclarationFromPath(declarationPath.item);

    const content: string[] = [];
    if (declaration) {
        const code = render.renderDeclaration(declaration);
        if (code) {
            content.push("```wipple\n" + code + "\n```");
        }

        const renderedDocumentation = render.renderDocumentation(declaration);
        if (renderedDocumentation) {
            content.push(renderedDocumentation.docs);
        }
    }

    if (content.length === 0) {
        return null;
    }

    return {
        contents: {
            kind: "markdown",
            value: content.join("\n\n"),
        },
    };
});

connection.onCompletion(async (params) => {
    const uri = URI.parse(params.textDocument.uri);
    if (uri.scheme !== "file") {
        return null;
    }

    const document = documents.get(params.textDocument.uri);
    if (!document) {
        return null;
    }

    const position = document.offsetAt(params.position);

    const suggestions = render.renderSuggestionsAtCursor(getVisiblePath(uri.fsPath), position);

    return suggestions.map((suggestion): CompletionItem => {
        let kind: CompletionItemKind;
        switch (suggestion.kind) {
            case "type":
                kind = CompletionItemKind.Class;
                break;
            case "trait":
                kind = CompletionItemKind.Interface;
                break;
            case "typeParameter":
                kind = CompletionItemKind.TypeParameter;
                break;
            case "constant":
                kind = CompletionItemKind.Constant;
                break;
            case "variable":
                kind = CompletionItemKind.Variable;
                break;
            case "keyword":
                kind = CompletionItemKind.Keyword;
                break;
            case "operator":
                kind = CompletionItemKind.Operator;
                break;
        }

        return {
            kind,
            label: suggestion.name,
            detail: suggestion.code ?? undefined,
            documentation: suggestion.docs?.docs,
        };
    });
});

connection.onDefinition(async (params) => {
    const uri = URI.parse(params.textDocument.uri);
    if (uri.scheme !== "file") {
        return null;
    }

    const document = documents.get(params.textDocument.uri);
    if (!document) {
        return null;
    }

    const position = document.offsetAt(params.position);

    const declarationPath = render.getPathAtCursor(getVisiblePath(uri.fsPath), position);
    if (!declarationPath) {
        return null;
    }

    const declaration = render.getDeclarationFromPath(declarationPath.item);
    if (!declaration) {
        return null;
    }

    const declarationUri = `file://${declaration.info.location.path}`;

    const declarationDocument = documents.get(declarationUri);
    if (!declarationDocument) {
        return null;
    }

    return {
        uri: declarationUri,
        range: {
            start: declarationDocument.positionAt(declaration.info.location.span.start),
            end: declarationDocument.positionAt(declaration.info.location.span.end),
        },
    };
});

documents.listen(connection);
connection.listen();

const compileAll = async (
    activeFile: string,
    activeCode: string,
): Promise<compiler.WithInfo<compiler.Info, compiler.Diagnostic>[]> => {
    const activeDir = path.normalize(path.dirname(activeFile));

    const sourcePaths = (await fs.readdir(activeDir))
        .map((file) => path.join(activeDir, file))
        .filter((file) => path.extname(file) === ".wipple");

    const sources = await Promise.all(
        sourcePaths.map(
            async (sourcePath): Promise<compiler.File> => ({
                path: sourcePath,
                visiblePath: getVisiblePath(sourcePath),
                code:
                    path.normalize(activeFile) === path.normalize(sourcePath)
                        ? activeCode
                        : await fs.readFile(sourcePath, "utf8"),
            }),
        ),
    );

    const result = compiler.compile(sources, dependencies);
    render.update(result.interface, [result.library, ...libraries], result.ide);

    return result.diagnostics;
};
