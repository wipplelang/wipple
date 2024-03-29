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
import * as compiler from "wipple-compiler";
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

    const info = render.getInfoAtCursor("top-level", position);
    if (!info) {
        return null;
    }

    const content: string[] = [];

    const declaration = render.getDeclarationFromInfo(info);
    const code = declaration ? render.renderDeclaration(declaration) : null;

    if (code) {
        content.push("```wipple\n" + code + "\n```");
    }

    const renderedDocumentation = render.renderDocumentation({ info, item: null });
    if (renderedDocumentation) {
        content.push(renderedDocumentation.docs);
    }

    return {
        contents: {
            kind: "markdown",
            value: content.join("\n\n"),
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

    const dirs = [
        activeDir,
        // TODO: Link against library, not source files
        ...linkDirs.filter((dir) => path.normalize(dir) !== activeDir),
    ];

    const sourcePaths = (
        await Promise.all(
            dirs.map(async (dir) => (await fs.readdir(dir)).map((file) => path.join(dir, file))),
        )
    )
        .flatMap((files) => files)
        .filter((file) => path.extname(file) === ".wipple");

    const sources = await Promise.all(
        sourcePaths.map(
            async (sourcePath): Promise<compiler.File> => ({
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

    // TODO: Support dependencies
    const dependencies = null;

    const result = compiler.compile(sources, dependencies);
    render.update(result.interface, [result.library]); // TODO: Dependencies

    return result.diagnostics;
};
