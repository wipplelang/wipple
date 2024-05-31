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
import { evaluate, InterpreterError, IoRequest, Executable } from "wipple-interpreter";
import { Render } from "wipple-render";
import builder from "junit-report-builder";
import { marked } from "marked";

interface DoctestItem {
    file: string;
    code: string;
    output: string | null;
}

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
                    console.error(renderDiagnostics(result.diagnostics, result.interface));
                    process.exit(1);
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
                    console.error(renderDiagnostics(result.diagnostics, result.interface));
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

                await runExecutable(executable);
            },
        }),
        doctest: command({
            name: "doctest",
            args: {
                dependencyInterfacePath: option({ long: "dependency", type: optional(File) }),
                dependencyLibrariesPaths: multioption({ long: "link", type: array(File) }),
                filePaths: restPositionals({ type: File }),
            },
            handler: async ({ dependencyInterfacePath, dependencyLibrariesPaths, filePaths }) => {
                const dependencies: compiler.Interface | null = dependencyInterfacePath
                    ? JSON.parse(fs.readFileSync(dependencyInterfacePath, "utf8"))
                    : null;

                const libraries: compiler.linker_UnlinkedLibrary[] = dependencyLibrariesPaths.map(
                    (path) => JSON.parse(fs.readFileSync(path, "utf8")),
                );

                const items = filePaths.flatMap((filePath) => {
                    const file = fs.readFileSync(filePath, "utf8");

                    filePath = `${path.basename(path.dirname(filePath))}/${path.basename(
                        filePath,
                    )}`;

                    const codeBlocks = marked.lexer(file).flatMap((token) => {
                        if (token.type === "code") {
                            return [
                                {
                                    lang: token.lang as string,
                                    text: token.text as string,
                                },
                            ];
                        }

                        return [];
                    });

                    const items: DoctestItem[] = [];
                    while (codeBlocks.length > 0) {
                        const { lang, text: code } = codeBlocks.shift()!;
                        if (lang === "wipple") {
                            const next = codeBlocks.shift();
                            if (!next) break;
                            if (!next.lang) continue;

                            switch (next.lang) {
                                case "wipple-output": {
                                    items.push({
                                        file: filePath,
                                        code,
                                        output: next.text,
                                    });

                                    break;
                                }
                                case "wipple": {
                                    items.push({
                                        file: filePath,
                                        code,
                                        output: null,
                                    });

                                    codeBlocks.unshift(next);

                                    break;
                                }
                                default: {
                                    items.push({
                                        file: filePath,
                                        code,
                                        output: null,
                                    });

                                    break;
                                }
                            }
                        }
                    }

                    return items;
                });

                const testBuilder = builder.newBuilder();
                testBuilder.testSuite().name("doctest");

                let success = true;
                for (let { file, code, output: expectedOutput } of items) {
                    code = code
                        .split("\n")
                        .map((line) => {
                            const match = /^#(.*)$/.exec(line);
                            return match ? match[1] : line;
                        })
                        .join("\n");

                    const sources = [
                        {
                            path: "example",
                            visiblePath: "example",
                            code,
                        },
                    ];

                    let output = "";

                    const result = compiler.compile(sources, dependencies);

                    if (result.diagnostics.length > 0) {
                        output += renderDiagnostics(result.diagnostics, result.interface) + "\n";
                    } else {
                        const linkOutput = compiler.link([...libraries, result.library]);

                        if (linkOutput) {
                            await runExecutable(
                                linkOutput,
                                (message) => {
                                    output += message + "\n";
                                },
                                (message) => {
                                    output += `error: ${message}\n`;
                                },
                            );
                        }
                    }

                    if (expectedOutput == null && result.diagnostics.length > 0) {
                        testBuilder
                            .testCase()
                            .name(file)
                            .failure(`compilation failed with no expected output:\n${output}`);

                        success = false;
                        continue;
                    } else if (expectedOutput) {
                        output = output.trim();
                        expectedOutput = expectedOutput.trim();

                        if (output !== expectedOutput) {
                            testBuilder
                                .testCase()
                                .name(file)
                                .failure(`expected:\n${expectedOutput}\n\nactual:\n${output}`);

                            success = false;
                            continue;
                        }
                    }

                    testBuilder.testCase().name(file);
                }

                console.log(testBuilder.build());

                if (!success) {
                    process.exit(1);
                }
            },
        }),
    },
});

run(app, process.argv.slice(2));

const renderDiagnostics = (
    diagnostics: compiler.WithInfo<compiler.Info, compiler.Diagnostic>[],
    interface_: compiler.Interface,
) => {
    const render = new Render();
    render.update(interface_, []);

    return diagnostics
        .flatMap((diagnostic) => {
            const renderedDiagnostic = render.renderDiagnostic(diagnostic);
            if (!renderedDiagnostic) {
                return [];
            }

            return [render.renderDiagnosticToDebugString(renderedDiagnostic)];
        })
        .join("\n");
};

const runExecutable = async (
    executable: Executable,
    handleOutput: (message: string) => void = console.log,
    handleError: (message: string) => void = console.error,
) => {
    const handleIo = async (request: IoRequest) => {
        switch (request.type) {
            case "display": {
                handleOutput(request.message);
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
                throw new InterpreterError("UI is not supported outside the Wipple Playground");
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
            handleError(error.message);
        } else {
            throw error;
        }
    }
};
