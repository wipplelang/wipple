import * as vscode from "vscode";
import { LanguageClient, TransportKind } from "vscode-languageclient/node";

let client;

export const activate = (context) => {
    let output = vscode.window.createOutputChannel("Wipple");
    output.appendLine("Wipple extension initialized");

    const serverOptions = {
        command: "wipple",
        args: ["lsp"],
        transport: TransportKind.stdio,
    };

    const clientOptions = {
        documentSelector: [{ scheme: "file", language: "wipple" }],
    };

    client = new LanguageClient(
        "wippleLanguageServer",
        "Wipple Language Server",
        serverOptions,
        clientOptions,
    );

    client.start();
};

export const deactivate = async () => {
    await client?.stop();
};
