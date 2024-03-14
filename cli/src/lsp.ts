import { TextDocument } from "vscode-languageserver-textdocument";
import {
    Diagnostic,
    DiagnosticSeverity,
    DocumentDiagnosticReportKind,
    ProposedFeatures,
    TextDocumentSyncKind,
    TextDocuments,
    createConnection,
} from "vscode-languageserver/node.js";
import { URI } from "vscode-uri";
import fs from "fs/promises";
import path from "path";
import { WithInfo, compile, linker, main } from "wipple-compiler";
import { Render, RenderedDiagnostic } from "wipple-render";

export const runLanguageServer = () => {
    const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

    const render = new Render();
    let diagnostics: RenderedDiagnostic[] = [];

    const connection = createConnection(ProposedFeatures.all);

    connection.onInitialize((_params) => ({
        capabilities: {
            textDocumentSync: TextDocumentSyncKind.Full,
            diagnosticProvider: {
                interFileDependencies: true,
                workspaceDiagnostics: true,
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

        diagnostics = (await compileAll(path.dirname(uri.fsPath))).flatMap((diagnostic) => {
            const rendered = render.renderDiagnostic(diagnostic);
            return rendered ? [rendered] : [];
        });

        connection.languages.diagnostics.refresh();
    });

    connection.languages.diagnostics.on(async (_params) => ({
        kind: DocumentDiagnosticReportKind.Full,
        items: diagnostics.map((diagnostic): Diagnostic => {
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
    }));

    documents.listen(connection);
    connection.listen();
};

const compileAll = async (dir: string): Promise<WithInfo<main.Info, main.Diagnostic>[]> => {
    const sourcePaths = (await fs.readdir(dir)).map((file) => path.join(dir, file));

    const sources = await Promise.all(
        sourcePaths.map(async (sourcePath) => ({
            path: sourcePath,
            visiblePath: `${path.basename(path.dirname(sourcePath))}/${path.basename(sourcePath)}`,
            code: await fs.readFile(sourcePath, "utf8"),
        })),
    );

    // TODO: Support dependencies
    const dependencies: linker.UnlinkedLibrary[] = [];

    const result = compile(sources, dependencies);
    return result.diagnostics;
};
