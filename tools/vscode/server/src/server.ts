import {
    createConnection,
    TextDocuments,
    ProposedFeatures,
    InitializeParams,
    DidChangeConfigurationNotification,
    TextDocumentSyncKind,
    InitializeResult,
    Diagnostic as LspDiagnostic,
    DiagnosticSeverity,
} from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";
import execa from "execa";
import lineColumn from "line-column";
import { Program, Diagnostic } from "./models";

type LineColumnFinder = ReturnType<typeof lineColumn>;

const debug = process.env.WIPPLE_LSP_DEVELOPMENT === "1";

const connection = createConnection(ProposedFeatures.all);
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);
let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
connection.onInitialize((params: InitializeParams) => {
    const capabilities = params.capabilities;

    hasConfigurationCapability = !!(
        capabilities.workspace && !!capabilities.workspace.configuration
    );
    hasWorkspaceFolderCapability = !!(
        capabilities.workspace && !!capabilities.workspace.workspaceFolders
    );

    const result: InitializeResult = {
        capabilities: {
            textDocumentSync: TextDocumentSyncKind.Incremental,
            // semanticTokensProvider: {
            //     documentSelector: null,
            //     legend: {
            //         tokenTypes: [],
            //         tokenModifiers: [],
            //     },
            // },
            // completionProvider: {
            //     resolveProvider: true,
            // },
        },
    };

    if (hasWorkspaceFolderCapability) {
        result.capabilities.workspace = {
            workspaceFolders: {
                supported: true,
            },
        };
    }
    return result;
});

connection.onInitialized(() => {
    if (hasConfigurationCapability) {
        connection.client.register(DidChangeConfigurationNotification.type, undefined);
    }
});

documents.onDidChangeContent(async (change) => {
    const source = change.document.getText();

    const program = debug ? "bash" : "/usr/bin/env";

    const args = debug
        ? ["-c", `cd '${process.cwd()}' && target/debug/wipple compile - --target analysis`]
        : ["wipple", "compile", "-", "--target", "analysis"];

    let output: execa.ExecaReturnBase<string>;
    try {
        output = await execa(program, args, {
            input: source,
        });
    } catch (e) {
        output = e as execa.ExecaError;
    }

    const result: { program: Program; diagnostics: Diagnostic[] } = JSON.parse(output.stdout);

    const lc = lineColumn(source);

    const diagnostics = convertDiagnostics(lc, result.diagnostics);
    connection.sendDiagnostics({ uri: change.document.uri, diagnostics });
});

const convertDiagnostics = (lc: LineColumnFinder, diagnostics: Diagnostic[]) =>
    diagnostics.flatMap((diagnostic) => {
        const primaryNote = diagnostic.notes[0];
        if (primaryNote.span.path.value !== "stdin") {
            return [];
        }

        let severity: DiagnosticSeverity;
        switch (primaryNote.level) {
            case "Primary":
                switch (diagnostic.level) {
                    case "Error":
                        severity = 1;
                        break;
                    case "Warning":
                        severity = 2;
                        break;
                    case "Note":
                        severity = 3;
                        break;
                }
                break;
            case "Secondary":
                severity = 4;
                break;
        }

        const start = lc.fromIndex(primaryNote.span.start);
        const end = lc.fromIndex(primaryNote.span.end);
        if (!start || !end) return [];

        const lspDiagnostic: LspDiagnostic = {
            severity,
            message: `${diagnostic.message}\n${primaryNote.message}`,
            // TODO: relatedInformation with secondary notes
            range: {
                start: { line: start.line - 1, character: start.col - 1 },
                end: { line: end.line - 1, character: end.col - 1 },
            },
        };

        return [lspDiagnostic];
    });

documents.listen(connection);
connection.listen();
