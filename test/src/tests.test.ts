import fs from "fs";
import fsp from "fs/promises";
import path from "path";
import { compile, link } from "wipple-compiler";
import { evaluate, InterpreterError, IoRequest } from "wipple-interpreter";

Error.stackTraceLimit = Infinity;

let prefix = process.env.WIPPLE_TEST_PREFIX;
if (!prefix) {
    throw new Error("WIPPLE_TEST_PREFIX not set");
}

prefix = path.join(prefix, ".wipple");

const baseInterface = JSON.parse(
    fs.readFileSync(path.join(prefix, "base.wippleinterface"), "utf8")
);

const baseLibrary = JSON.parse(fs.readFileSync(path.join(prefix, "base.wipplelibrary"), "utf8"));

for (let file of fs.readdirSync("tests")) {
    if (path.extname(file) === ".wipple") {
        test(file, async () => {
            const code = await fsp.readFile(path.join("tests", file), "utf8");
            file = path.join("test/tests", file);

            const header = code.match(/\[([\w\s]+)\]/)?.[1];
            let shouldCompile: boolean;
            switch (header) {
                case "should compile": {
                    shouldCompile = true;
                    break;
                }
                case "should error": {
                    shouldCompile = false;
                    break;
                }
                default: {
                    throw new Error(
                        "expected test to begin with [should compile] or [should error]"
                    );
                }
            }

            const compileResult = compile([{ path: file, code }], baseInterface);

            if (compileResult.errors.length > 0) {
                expect(shouldCompile).toBeFalsy();

                expect({ errors: compileResult.errors }).toMatchSnapshot();
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
