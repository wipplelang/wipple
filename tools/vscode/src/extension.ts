import * as path from "path";
import * as vscode from "vscode";
import {
    DocumentSemanticsTokensSignature,
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient;

export const activate = (context: vscode.ExtensionContext) => {
    const serverOptions: ServerOptions = {
        run: {
            command: "wipple lsp",
        },
        debug: {
            command: "/bin/sh",
            args: [
                "-c",
                `cd ${path.join(
                    __dirname,
                    "../../.."
                )} && RUST_BACKTRACE=1 cargo run --bin wipple -- lsp`,
            ],
        },
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: "file", language: "wipple" }],
    };

    client = new LanguageClient(
        "wippleLanguageServer",
        "Wipple Language Server",
        serverOptions,
        clientOptions
    );

    client.start();
};

export const deactivate = () => client?.stop();
