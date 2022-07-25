import * as path from "path";
import { ExtensionContext } from "vscode";

import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient;

export const activate = (context: ExtensionContext) => {
    const serverModule = context.asAbsolutePath(path.join("server", "out", "server.js"));

    const debugOptions = {
        cwd: path.resolve(__dirname, "../../../.."),
        env: { WIPPLE_LSP_DEVELOPMENT: "1" },
        execArgv: ["--nolazy", "--inspect=6009"],
    };

    const serverOptions: ServerOptions = {
        run: {
            module: serverModule,
            transport: TransportKind.ipc,
        },
        debug: {
            module: serverModule,
            transport: TransportKind.ipc,
            options: debugOptions,
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

export const deactivate = (): Thenable<void> | undefined => {
    if (!client) {
        return undefined;
    }

    return client.stop();
};
