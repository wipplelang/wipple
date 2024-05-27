import * as vscode from "vscode";
import { LanguageClient, LanguageClientOptions, ServerOptions } from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export const activate = (context: vscode.ExtensionContext) => {
    const config = vscode.workspace.getConfiguration("wipple");

    const serverOptions: ServerOptions = {
        module: context.asAbsolutePath("dist/lsp.js"),
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: "file", language: "wipple" }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher("**/*.wipple"),
        },
        initializationOptions: {
            interface: config.get<string>("interface"),
            libraries: config.get<string[]>("libraries"),
        },
    };

    client = new LanguageClient("wipple", "Wipple Language Server", serverOptions, clientOptions);

    client.start();
};

export const deactivate = async () => {
    await client?.stop();
};
