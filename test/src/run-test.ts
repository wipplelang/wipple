import fs from "fs";
import path from "path";
import _ from "lodash";
import { compile, link } from "wipple-compiler";
import { evaluate, InterpreterError, IoRequest } from "wipple-interpreter";
import { Render } from "wipple-render";

Error.stackTraceLimit = Infinity;

const prefix = path.resolve(import.meta.dir, "../../.wipple");

const baseInterface = JSON.parse(
    fs.readFileSync(path.join(prefix, "base.wippleinterface"), "utf8"),
);

const baseLibrary = JSON.parse(fs.readFileSync(path.join(prefix, "base.wipplelibrary"), "utf8"));

export const runTest = async (
    file: string,
    code: string,
    compare: (left: any, right: any) => void,
    compareSnapshot: (value: any) => void,
) => {
    const header = code.match(/-- \[([\w\s]+)\]/)?.[1];
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
                "expected test to begin with [should compile], [should warn], or [should error]",
            );
        }
    }

    const compileResult = compile([{ path: file, visiblePath: file, code }], baseInterface);

    const render = new Render();
    render.update(compileResult.interface, [baseLibrary, compileResult.library]);

    let compiled = true;
    let compiledWithWarnings = false;
    const renderedDiagnostics = compileResult.diagnostics.map((diagnostic) => {
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
    });

    compare(shouldCompile, compiled);

    if (shouldWarn != null) {
        compare(shouldWarn, compiledWithWarnings);
    }

    compareSnapshot(renderedDiagnostics);

    if (!compiled) {
        return;
    }

    const executable = link([compileResult.library, baseLibrary]);

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

    compareSnapshot({ output });
};
