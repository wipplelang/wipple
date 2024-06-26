import * as vscode from "vscode";
import {
    Executable,
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export const activate = async (context: vscode.ExtensionContext) => {
    const config = vscode.workspace.getConfiguration("wipple");

    const run: Executable = {
        command: "wipple",
        args: ["lsp"],
        options: {
            env: {
                ...process.env,
                RUST_LOG: "debug",
            },
        },
    };

    const serverOptions: ServerOptions = {
        run,
        debug: run,
    };

    const traceOutputChannel = vscode.window.createOutputChannel("Wipple Language Server Trace");

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: "file", language: "wipple" }],
        traceOutputChannel,
        initializationOptions: {
            interface: config.get<string>("interface"),
            libraries: config.get<string[]>("libraries"),
        },
    };

    client = new LanguageClient(
        "wipple-language-server",
        "Wipple Language Server",
        serverOptions,
        clientOptions,
    );

    client.start();
};

export const deactivate = async () => {
    if (client?.isRunning()) {
        await client.stop();
    }
};
