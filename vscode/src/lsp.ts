import { TextDocument } from "vscode-languageserver-textdocument";
import {
    Diagnostic,
    DiagnosticSeverity,
    DocumentDiagnosticReportKind,
    ProposedFeatures,
    TextDocumentSyncKind,
    TextDocuments,
    createConnection,
} from "vscode-languageserver/node";
import { URI } from "vscode-uri";
import * as fs from "fs/promises";
import * as path from "path";
import { WithInfo, compile, main } from "wipple-compiler";
import { Render, RenderedDiagnostic } from "wipple-render";

const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

const render = new Render();
let diagnostics: RenderedDiagnostic[] = [];

const connection = createConnection(ProposedFeatures.all);

connection.onInitialize((_params) => ({
    capabilities: {
        textDocumentSync: TextDocumentSyncKind.Full,
        diagnosticProvider: {
            documentSelector: [{ language: "wipple" }],
            interFileDependencies: true,
            workspaceDiagnostics: false,
        },
    },
}));

connection.onInitialized(() => {
    console.log("Connection initialized");
});

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

documents.listen(connection);
connection.listen();

const compileAll = async (
    activeFile: string,
    activeCode: string,
): Promise<WithInfo<main.Info, main.Diagnostic>[]> => {
    const dir = path.dirname(activeFile);

    const sourcePaths = (await fs.readdir(dir))
        .map((file) => path.join(dir, file))
        .filter((file) => path.extname(file) === ".wipple");

    const sources = await Promise.all(
        sourcePaths.map(
            async (sourcePath): Promise<main.File> => ({
                path: sourcePath,
                visiblePath: `${path.basename(path.dirname(sourcePath))}/${path.basename(
                    sourcePath,
                )}`,
                code:
                    path.normalize(activeFile) === path.normalize(sourcePath)
                        ? activeCode
                        : await fs.readFile(sourcePath, "utf8"),
            }),
        ),
    );

    render.updateFiles(sources);

    // TODO: Support dependencies
    const dependencies = null;

    const result = compile(sources, dependencies);
    return result.diagnostics;
};
