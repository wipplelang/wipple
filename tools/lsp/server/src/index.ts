import { execa } from "execa";
import {
    createConnection,
    TextDocuments,
    ProposedFeatures,
    InitializeParams,
    DidChangeConfigurationNotification,
    TextDocumentSyncKind,
    InitializeResult,
    Diagnostic as LspDiagnostic,
} from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";
import { Program, Diagnostic } from "./models";

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

    const child = await execa("/usr/bin/env", ["wipple", "compile", "-", "--target", "analysis"], {
        input: source,
    });

    const success = child.exitCode === 0;

    if (success) {
        const result: { program: Program; diagnostics: Diagnostic[] } = JSON.parse(child.stdout);

        console.warn("RESULT:", result);

        const diagnostics = convertDiagnostics(result.diagnostics);
        connection.sendDiagnostics({ uri: change.document.uri, diagnostics });
    } else {
    }
});

const convertDiagnostics = (diagnostics: Diagnostic[]) => {
    const lspDiagnostics: LspDiagnostic[] = [];

    return lspDiagnostics;
};

documents.listen(connection);
connection.listen();
