import fs from "fs";
import fsp from "fs/promises";
import path from "path";
import _ from "lodash";
import { compile, link, renderDiagnostics } from "wipple-compiler";
import { evaluate, InterpreterError, IoRequest } from "wipple-interpreter";

Error.stackTraceLimit = Infinity;

let prefix = process.env.WIPPLE_TEST_PREFIX;
if (!prefix) {
    throw new Error("WIPPLE_TEST_PREFIX not set");
}

prefix = path.join(prefix, ".wipple");

const baseInterface = JSON.parse(
    fs.readFileSync(path.join(prefix, "base.wippleinterface"), "utf8"),
);

const baseLibrary = JSON.parse(fs.readFileSync(path.join(prefix, "base.wipplelibrary"), "utf8"));

for (let file of fs.readdirSync("tests").sort()) {
    if (path.extname(file) === ".wipple") {
        test(file, async () => {
            const code = await fsp.readFile(path.join("tests", file), "utf8");
            file = path.join("test/tests", file);

            const header = code.match(/\[([\w\s]+)\]/)?.[1];
            let shouldCompile: boolean;
            let shouldWarn: boolean | undefined;
            switch (header) {
                case "should compile": {
                    shouldCompile = true;
                    shouldWarn = false;
                    break;
                }
                case "should warn": {
                    shouldCompile = true;
                    shouldWarn = true;
                    break;
                }
                case "should error": {
                    shouldCompile = false;
                    shouldWarn = undefined;
                    break;
                }
                default: {
                    throw new Error(
                        "expected test to begin with -- [should compile], [should warn], or [should error]",
                    );
                }
            }

            const compileResult = compile([{ path: file, visiblePath: file, code }], baseInterface);

            const renderedDiagnostics = renderDiagnostics(
                compileResult.diagnostics,
                compileResult.interface,
                compileResult.library,
                (path) => (path === file ? code : ""),
            ).map((diagnostic) => {
                // Hide the actual path from the snapshot so tests can be
                // reproduced across machines

                delete diagnostic.primaryLabel.path;

                for (const label of diagnostic.secondaryLabels) {
                    delete label.path;
                }

                return diagnostic;
            });

            const [errors, warnings] = _.partition(
                renderedDiagnostics,
                (diagnostic) => diagnostic.error,
            );

            const compiled = errors.length === 0;
            const compiledWithWarnings = warnings.length > 0;

            expect(shouldCompile).toBe(compiled);

            if (shouldWarn != null) {
                expect(shouldWarn).toBe(compiledWithWarnings);
            }

            expect({ errors, warnings }).toMatchSnapshot();

            if (!compiled) {
                return;
            }

            const linkResult = link([compileResult.library, baseLibrary]);

            if (linkResult.Err) {
                expect(shouldCompile).toBeFalsy();
                expect({ errors: linkResult.Err }).toMatchSnapshot();
                return;
            }

            expect(shouldCompile).toBeTruthy();

            const executable = linkResult.Ok;

            let output = "";
            const handleIo = async (request: IoRequest) => {
                switch (request.type) {
                    case "display": {
                        output += request.message + "\n";
                        request.completion();
                        break;
                    }
                    default: {
                        throw new Error("not supported in tests");
                    }
                }
            };

            try {
                await evaluate(executable, {
                    io: handleIo,
                });
            } catch (error) {
                if (error instanceof InterpreterError) {
                    output += error.message + "\n";
                } else {
                    throw error;
                }
            }

            expect({ output }).toMatchSnapshot();
        });
    }
}
