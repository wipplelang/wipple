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
    log.appendLine(`PATH: ${path}`);

    const serverOptions: ServerOptions = {
        module: path,
        transport: TransportKind.ipc,
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: "file", language: "wipple" }],
    };

    client = new LanguageClient(
        "wippleLanguageServer",
        "Wipple Language Server",
        serverOptions,
        clientOptions,
    );

    await client.start();
};

export const deactivate = async () => {
    await client?.dispose();
    client = undefined;
};
