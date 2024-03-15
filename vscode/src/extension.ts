import * as vscode from "vscode";
import { LanguageClient, LanguageClientOptions, ServerOptions } from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export const activate = (context: vscode.ExtensionContext) => {
    const serverOptions: ServerOptions = {
        module: context.asAbsolutePath("dist/lsp.js"),
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: "file", language: "wipple" }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher("**/*.wipple"),
        },
    };

    client = new LanguageClient("wipple", "Wipple Language Server", serverOptions, clientOptions);

    client.start();
};

export const deactivate = async () => {
    await client?.stop();
};
