import fs from "fs";
import path from "path";
import {
    command,
    run,
    restPositionals,
    multioption,
    array,
    subcommands,
    option,
    string,
    optional,
    positional,
} from "cmd-ts";
import { File } from "cmd-ts/batteries/fs";
import { compile, link } from "wipple-compiler";
import { evaluate, InterpreterError } from "wipple-interpreter";

const shebang = `#!/usr/bin/env wipple run\n`;

const app = subcommands({
    name: "wipple",
    cmds: {
        compile: command({
            name: "compile",
            args: {
                dependencyPaths: multioption({ long: "dependencies", type: array(File) }),
                outputInterfacePath: option({ long: "interface", type: optional(string) }),
                outputLibraryPath: option({ long: "library", type: optional(string) }),
                sourcePaths: restPositionals({ type: File }),
            },
            handler: async ({
                dependencyPaths,
                outputInterfacePath,
                outputLibraryPath,
                sourcePaths,
            }) => {
                const sources = sourcePaths.map((path) => {
                    const code = fs.readFileSync(path, "utf8");
                    return { path, code };
                });

                const dependencies = dependencyPaths.map((path) =>
                    JSON.parse(fs.readFileSync(path, "utf8"))
                );

                const result = compile(sources, dependencies);

                if (result.errors.length > 0) {
                    console.error(result.errors);
                    process.exit(1);
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

                const executable = link(libraries);

                if (!executable) {
                    console.error("failed to link libraries");
                    process.exit(1);
                }

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

                try {
                    await evaluate(executable);
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
