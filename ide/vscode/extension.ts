import * as vscode from "vscode";
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

const log = vscode.window.createOutputChannel("Wipple Language Server");

export const activate = async (context: vscode.ExtensionContext) => {
    const path = context.asAbsolutePath("dist/wipple-lsp/index.js");
    log.appendLine(`LSP path: ${path}`);

    const serverOptions: ServerOptions = {
        module: path,
        transport: TransportKind.ipc,
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: "file", language: "wipple" }],
        markdown: {
            isTrusted: {
                enabledCommands: ["wipple.explainDiagnostic", "wipple.explain"],
            },
        },
    };

    client = new LanguageClient(
        "wippleLanguageServer",
        "Wipple Language Server",
        serverOptions,
        clientOptions,
    );

    client.start();

    const renderTrace = async (documentUri: string, primary: any, secondary: any[]) => {
        if (!client) return "";

        const document = vscode.workspace.textDocuments.find(
            (document) => document.uri.toString() === documentUri,
        );

        if (document == null) return "";

        const getCodeBlock = (range: any, primary: boolean) => {
            const line = document.lineAt(range.start.line - 1);

            const annotation = primary ? "^" : "-";

            return (
                "```wipple\n" +
                line.text +
                "\n" +
                " ".repeat(range.start.column - 1) +
                annotation.repeat(range.end.column - range.start.column) +
                "\n```"
            );
        };

        let output = "";
        let counter = 1;

        const appendOutput = (message: any, primary: boolean) => {
            if (counter > 1) {
                output += "\n\n---\n\n";
            }

            output += `**${counter++}.**\n\n`;

            output += getCodeBlock(message.range, primary) + "\n\n" + message.message;
        };

        for (const message of secondary) {
            appendOutput(message, false);
        }

        if (primary != null) {
            appendOutput(primary, true);
        }

        return output;
    };

    const onDidChange = new vscode.EventEmitter<vscode.Uri>();

    context.subscriptions.push(
        vscode.workspace.registerTextDocumentContentProvider("wipple-diagnostic", {
            onDidChange: onDidChange.event,
            provideTextDocumentContent: async (uri: vscode.Uri) => {
                if (!client) return "";

                const query = new URLSearchParams(uri.query);
                const documentUri = query.get("uri")!;
                const diagnosticIndex = parseFloat(query.get("index")!);

                const diagnostic: any = await client.sendRequest("wipple/getDiagnostic", {
                    uri: documentUri,
                    index: diagnosticIndex,
                });

                if (diagnostic == null) return "";

                const { primary, secondary } = diagnostic;

                return await renderTrace(documentUri, primary, secondary);
            },
        }),
        vscode.workspace.registerTextDocumentContentProvider("wipple-trace", {
            onDidChange: onDidChange.event,
            provideTextDocumentContent: async (uri: vscode.Uri) => {
                if (!client) return "";

                const query = new URLSearchParams(uri.query);
                const documentUri = query.get("uri")!;
                const line = parseFloat(query.get("line")!);
                const column = parseFloat(query.get("column")!);

                const trace: any = await client.sendRequest("wipple/getTrace", {
                    uri: documentUri,
                    line,
                    column,
                });

                if (trace == null) {
                    vscode.window.showErrorMessage("No additional information for this code.");
                    return "";
                }

                return await renderTrace(documentUri, undefined, trace);
            },
        }),
        vscode.commands.registerCommand(
            "wipple.explainDiagnostic",
            async ({ uri, index }: { uri: string; index: number }) => {
                const diagnosticUri = vscode.Uri.parse(
                    `wipple-diagnostic:/?uri=${encodeURIComponent(uri)}&index=${index}`,
                );

                onDidChange.fire(diagnosticUri);
                vscode.commands.executeCommand("markdown.showPreviewToSide", diagnosticUri);
                vscode.commands.executeCommand("markdown.refreshPreview", diagnosticUri);
            },
        ),
        vscode.commands.registerCommand(
            "wipple.explain",
            async ({ uri, line, column }: { uri: string; line: number; column: number }) => {
                const traceUri = vscode.Uri.parse(
                    `wipple-trace:/?uri=${encodeURIComponent(uri)}&line=${line}&column=${column}`,
                );

                onDidChange.fire(traceUri);
                vscode.commands.executeCommand("markdown.showPreviewToSide", traceUri);
                vscode.commands.executeCommand("markdown.refreshPreview", traceUri);
            },
        ),
    );
};

export const deactivate = async () => {
    await client?.dispose();
    client = undefined;
};
