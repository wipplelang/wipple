import fs from "fs";
import path from "path";
import {
    command,
    run,
    restPositionals,
    subcommands,
    option,
    string,
    optional,
    positional,
} from "cmd-ts";
import { File } from "cmd-ts/batteries/fs";
import { compile, link, renderDiagnostics, colorizeDiagnostics } from "wipple-compiler";
import { evaluate, InterpreterError, IoRequest } from "wipple-interpreter";

Error.stackTraceLimit = Infinity;

const shebang = `#!/usr/bin/env wipple run\n`;

const app = subcommands({
    name: "wipple",
    cmds: {
        compile: command({
            name: "compile",
            args: {
                dependencyPath: option({ long: "dependency", type: optional(File) }),
                outputInterfacePath: option({ long: "interface", type: optional(string) }),
                outputLibraryPath: option({ long: "library", type: optional(string) }),
                sourcePaths: restPositionals({ type: File }),
            },
            handler: async ({
                dependencyPath,
                outputInterfacePath,
                outputLibraryPath,
                sourcePaths,
            }) => {
                const sources = sourcePaths.map((path) => {
                    const code = fs.readFileSync(path, "utf8");
                    return { path, code };
                });

                const dependencies = dependencyPath
                    ? JSON.parse(fs.readFileSync(dependencyPath, "utf8"))
                    : null;

                const result = compile(sources, dependencies);

                if (result.diagnostics.length > 0) {
                    const sourceCodeForFile = (file: string) => {
                        try {
                            return fs.readFileSync(file, "utf8");
                        } catch {
                            return "";
                        }
                    };

                    const renderedDiagnostics = renderDiagnostics(
                        result.diagnostics,
                        result.interface,
                        result.library,
                        sourceCodeForFile
                    );

                    const output = colorizeDiagnostics(renderedDiagnostics, sourceCodeForFile);

                    console.log(output);

                    if (renderedDiagnostics.some((diagnostic: any) => diagnostic.error)) {
                        process.exit(1);
                    }
                }

                if (outputInterfacePath) {
                    fs.mkdirSync(path.dirname(outputInterfacePath), { recursive: true });
                    fs.writeFileSync(
                        outputInterfacePath,
                        JSON.stringify(result.interface, null, 4)
                    );
                }

                if (outputLibraryPath) {
                    fs.mkdirSync(path.dirname(outputLibraryPath), { recursive: true });
                    fs.writeFileSync(outputLibraryPath, JSON.stringify(result.library, null, 4));
                }
            },
        }),
        link: command({
            name: "link",
            args: {
                outputExecutablePath: option({ long: "output", short: "o", type: string }),
                libraryPaths: restPositionals({ type: File }),
            },
            handler: async ({ libraryPaths, outputExecutablePath }) => {
                const libraries = libraryPaths.map((path) =>
                    JSON.parse(fs.readFileSync(path, "utf8"))
                );

                const result = link(libraries);

                if (result.Err) {
                    console.error("failed to link libraries:", result.Err);
                    process.exit(1);
                }

                const executable = result.Ok;

                const output = shebang + JSON.stringify(executable, null, 4);

                fs.mkdirSync(path.dirname(outputExecutablePath), { recursive: true });
                fs.writeFileSync(outputExecutablePath, output);
                fs.chmodSync(outputExecutablePath, 0o755);
            },
        }),
        run: command({
            name: "run",
            args: {
                executablePath: positional({ type: File }),
            },
            handler: async ({ executablePath }) => {
                const executable = JSON.parse(
                    fs.readFileSync(executablePath, "utf8").replace(shebang, "")
                );

                const handleIo = async (request: IoRequest) => {
                    switch (request.type) {
                        case "display": {
                            console.log(request.message);
                            request.completion();
                            break;
                        }
                        case "prompt": {
                            throw new Error("TODO");
                        }
                        case "choice": {
                            throw new Error("TODO");
                        }
                        case "ui": {
                            throw new InterpreterError(
                                "UI is not supported outside the Wipple Playground"
                            );
                        }
                        case "sleep": {
                            setTimeout(request.completion, request.ms);
                            break;
                        }
                    }
                };

                try {
                    await evaluate(executable, {
                        io: handleIo,
                    });
                } catch (error) {
                    if (error instanceof InterpreterError) {
                        console.error(error.message);
                    } else {
                        throw error;
                    }
                }
            },
        }),
    },
});

run(app, process.argv.slice(2));
