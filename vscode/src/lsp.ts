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

let rootDir = ".";
let linkDirs: string[] = [];

connection.onInitialize((params) => {
    const rootUri = params.workspaceFolders?.[0]?.uri;
    if (rootUri) {
        rootDir = URI.parse(rootUri).fsPath;
    }

    if (params.initializationOptions?.linkDirs) {
        linkDirs = params.initializationOptions.linkDirs.map((dir: string) =>
            path.resolve(rootDir, dir),
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
        },
    };
});

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

    const info = render.getInfoAtCursor(uri.fsPath, position);
    if (!info) {
        return null;
    }

    const docs = render.renderDocumentation({ info, item: null });

    if (!docs) {
        return null;
    }

    return {
        contents: {
            kind: "markdown",
            value:
                "```json\n" +
                JSON.stringify(docs.attributes, null, 4) +
                "\n```" +
                "\n\n" +
                docs.docs,
        },
    };
});

documents.listen(connection);
connection.listen();

const compileAll = async (
    activeFile: string,
    activeCode: string,
): Promise<WithInfo<main.Info, main.Diagnostic>[]> => {
    const dirs = [path.dirname(activeFile), ...linkDirs];

    const sourcePaths = (
        await Promise.all(
            dirs.map(async (dir) => (await fs.readdir(dir)).map((file) => path.join(dir, file))),
        )
    )
        .flatMap((files) => files)
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

    render.updateInterface(result.interface);
    render.updateLibraries([result.library]); // FIXME: Incorporate dependencies

    return result.diagnostics;
};
