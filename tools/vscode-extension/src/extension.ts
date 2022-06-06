import * as path from "path";
import * as vscode from "vscode";
import { LanguageClient, ServerOptions, LanguageClientOptions } from "vscode-languageclient/node";
import * as which from "which";

export async function activate(context: vscode.ExtensionContext) {
    const args = ["lsp"];

    const serverOptions: ServerOptions = {
        run: {
            command: await which("wipple"),
            args,
        },
        debug: {
            command: path.join(__dirname, "../../../target/debug/wipple"),
            args,
        },
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: ["wipple"],
    };

    const client = new LanguageClient("Wipple", serverOptions, clientOptions);
    await client.start();
    context.subscriptions.push({ dispose: client.stop });
}
