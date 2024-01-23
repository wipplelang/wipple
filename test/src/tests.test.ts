import { test, expect } from "bun:test";
import fs from "fs";
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

for (const file of fs.readdirSync("tests")) {
    if (path.extname(file) === ".wipple") {
        test(file, async () => {
            const code = fs.readFileSync(path.join("tests", file), "utf8");

            const compileResult = compile([{ path: file, code }], baseInterface);

            if (compileResult.errors.length > 0) {
                expect({ errors: compileResult.errors }).toMatchSnapshot();
                return;
            }

            const linkResult = link([compileResult.library, baseLibrary]);

            if (linkResult.Err) {
                expect({ errors: linkResult.Err }).toMatchSnapshot();
                return;
            }

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
