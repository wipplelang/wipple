import { execa } from "execa";
import * as vscode from "vscode";
import {
    Executable,
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
} from "vscode-languageclient/node";
import path from "path";
import os from "os";
import fs from "fs";

const buildDir = path.join(
    fs.realpathSync(os.tmpdir()),
    `wipple-vscode-${Math.floor(Math.random() * Number.MAX_SAFE_INTEGER)
        .toString(16)
        .padStart(16, "0")
        .substring(4)}`,
);

let client: LanguageClient | undefined;

export const activate = async (context: vscode.ExtensionContext) => {
    const workspacePath = vscode.workspace.workspaceFolders?.[0].uri.fsPath;
    if (!workspacePath) {
        return;
    }

    const config = vscode.workspace.getConfiguration("wipple");
    const wipplecPath = config.get<string>("wipplecPath") || "wipplec";
    const wipplePath = config.get<string>("wipplePath") || "wipple";

    const { stdout: projectConfigStdout } = await execa(wipplePath, [
        "config",
        workspacePath,
        "--build-dir",
        buildDir,
        ...(wipplecPath ? ["--compiler", wipplecPath] : []),
    ]);

    const projectConfig = JSON.parse(projectConfigStdout);

    const run: Executable = {
        command: config.get<string>("wipplecPath") || "wipplec",
        args: ["lsp"],
        options: {
            env: {
                ...process.env,
                RUST_LOG: "debug",
                RUST_BACKTRACE: "1",
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
            project: projectConfig,
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

    if (fs.existsSync(buildDir)) {
        fs.rmdirSync(buildDir, { recursive: true });
    }
};
