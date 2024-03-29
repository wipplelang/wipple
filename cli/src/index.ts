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
    multioption,
    array,
} from "cmd-ts";
import { File } from "cmd-ts/batteries/fs";
import * as compiler from "wipple-compiler";
import { evaluate, InterpreterError, IoRequest } from "wipple-interpreter";

Error.stackTraceLimit = Infinity;

const stringify = (value: any) =>
    process.env.WIPPLE_DEBUG_PRETTY ? JSON.stringify(value, null, 4) : JSON.stringify(value);

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
                const sources = sourcePaths.map(
                    (sourcePath): compiler.File => ({
                        path: sourcePath,
                        visiblePath: `${path.basename(path.dirname(sourcePath))}/${path.basename(
                            sourcePath,
                        )}`,
                        code: fs.readFileSync(sourcePath, "utf8"),
                    }),
                );

                const dependencies: compiler.Interface | null = dependencyPath
                    ? JSON.parse(fs.readFileSync(dependencyPath, "utf8"))
                    : null;

                const result = compiler.compile(sources, dependencies);

                if (result.diagnostics.length > 0) {
                    console.log(JSON.stringify(result.diagnostics, null, 4));
                }

                if (outputInterfacePath) {
                    fs.mkdirSync(path.dirname(outputInterfacePath), { recursive: true });
                    fs.writeFileSync(outputInterfacePath, stringify(result.interface));
                }

                if (outputLibraryPath) {
                    fs.mkdirSync(path.dirname(outputLibraryPath), { recursive: true });
                    fs.writeFileSync(outputLibraryPath, stringify(result.library));
                }
            },
        }),
        "bundle-for-playground": command({
            name: "bundle-for-playground",
            args: {
                dependencyInterfacePath: option({ long: "dependency", type: optional(File) }),
                dependencyLibrariesPaths: multioption({ long: "link", type: array(File) }),
                outputPath: option({ long: "output", short: "o", type: string }),
                sourcePaths: restPositionals({ type: File }),
            },
            handler: async ({
                dependencyInterfacePath,
                dependencyLibrariesPaths,
                outputPath,
                sourcePaths,
            }) => {
                const sources = sourcePaths.map((sourcePath) => ({
                    path: sourcePath,
                    visiblePath: `${path.basename(path.dirname(sourcePath))}/${path.basename(
                        sourcePath,
                    )}`,
                    code: fs.readFileSync(sourcePath, "utf8"),
                }));

                const dependencies: compiler.Interface | null = dependencyInterfacePath
                    ? JSON.parse(fs.readFileSync(dependencyInterfacePath, "utf8"))
                    : null;

                const libraries: compiler.linker_UnlinkedLibrary[] = dependencyLibrariesPaths.map(
                    (path) => JSON.parse(fs.readFileSync(path, "utf8")),
                );

                const result = compiler.compile(sources, dependencies);

                if (result.diagnostics.length > 0) {
                    console.log(JSON.stringify(result.diagnostics, null, 4));
                    process.exit(1);
                }

                const output = {
                    interface: result.interface,
                    libraries: [...libraries, result.library],
                };

                fs.mkdirSync(path.dirname(outputPath), { recursive: true });
                fs.writeFileSync(outputPath, stringify(output));
            },
        }),
        link: command({
            name: "link",
            args: {
                outputExecutablePath: option({ long: "output", short: "o", type: string }),
                libraryPaths: restPositionals({ type: File }),
            },
            handler: async ({ libraryPaths, outputExecutablePath }) => {
                const libraries = libraryPaths.map(
                    (path): compiler.linker_UnlinkedLibrary =>
                        JSON.parse(fs.readFileSync(path, "utf8")),
                );

                const executable = compiler.link(libraries);

                if (!executable) {
                    console.error("linking failed");
                    process.exit(1);
                }

                const output = shebang + stringify(executable);

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
                    fs.readFileSync(executablePath, "utf8").replace(shebang, ""),
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
                                "UI is not supported outside the Wipple Playground",
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
                        debug: process.env.WIPPLE_DEBUG_INTERPRETER != null,
                        gc: () => {},
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
