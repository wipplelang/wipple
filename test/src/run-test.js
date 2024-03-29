import fs from "fs";
import path from "path";
import _ from "lodash";
import { compile, link } from "wipple-compiler";
import { evaluate, InterpreterError } from "wipple-interpreter";
import { Render } from "wipple-render";

Error.stackTraceLimit = Infinity;

const file = expect.getState().testPath;
const code = fs.readFileSync(file, "utf8");

test(path.basename(file), async () => {
    const prefix = path.resolve(process.cwd(), "../.wipple");

    const baseInterface = JSON.parse(
        fs.readFileSync(path.join(prefix, "base.wippleinterface"), "utf8"),
    );

    const baseLibrary = JSON.parse(
        fs.readFileSync(path.join(prefix, "base.wipplelibrary"), "utf8"),
    );

    const header = code.match(/-- \[([\w\s]+)\]/)?.[1];
    let shouldCompile;
    let shouldWarn;
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
                "expected test to begin with [should compile], [should warn], or [should error]",
            );
        }
    }

    const compileResult = compile(
        [
            {
                path: file,
                visiblePath: `${path.basename(path.dirname(file))}/${path.basename(file)}`,
                code,
            },
        ],
        baseInterface,
    );

    const render = new Render();
    render.update(compileResult.interface, [baseLibrary, compileResult.library]);

    let compiled = true;
    let compiledWithWarnings = false;
    const renderedDiagnostics = _.uniq(
        compileResult.diagnostics.map((diagnostic) => {
            const renderedDiagnostic = render.renderDiagnostic(diagnostic);
            if (!renderedDiagnostic) {
                throw new Error(`could not render diagnostic: ${diagnostic}`);
            }

            switch (renderedDiagnostic.severity) {
                case "error":
                    compiled = false;
                    break;
                case "warning":
                    compiledWithWarnings = true;
                    break;
                default:
                    break;
            }

            return render.renderDiagnosticToDebugString(renderedDiagnostic);
        }),
    );

    expect(shouldCompile).toBe(compiled);

    if (shouldWarn != null) {
        expect(shouldWarn).toBe(compiledWithWarnings);
    }

    expect(renderedDiagnostics).toMatchSnapshot();

    if (!compiled) {
        return;
    }

    const executable = link([compileResult.library, baseLibrary]);

    let output = "";
    const handleIo = async (request) => {
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
            debug: process.env.WIPPLE_DEBUG_INTERPRETER != null,
            io: handleIo,
            gc: () => {},
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
