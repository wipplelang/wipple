import { useEffect, useState } from "react";
import { useDebounceValue } from "usehooks-ts";
import { compile, link, renderDiagnostics } from "wipple-compiler";
import { evaluate, InterpreterError, IoRequest } from "wipple-interpreter";

export interface RunOptions {
    dependenciesPath: string;
}

type Diagnostic = "TODO";

type Output =
    | { type: "text"; text: string }
    | { type: "prompt"; prompt: string; onSubmit: (value: string) => Promise<boolean> };

export const Runner = (props: { children: string; options: RunOptions }) => {
    const [code, setCode] = useDebounceValue(props.children, 300);
    const [diagnostics, setDiagnostics] = useState<Diagnostic[]>([]);
    const [output, setOutput] = useState<Output[]>([]);

    const appendToOutput = (item: Output) => {
        setOutput((output) => [...output, item]);
    };

    const clearOutput = () => {
        setOutput([]);
    };

    useEffect(() => {
        setCode(props.children);
    }, [props.children]);

    useEffect(() => {
        console.log("running:", code);

        (async () => {
            try {
                const sources = [
                    {
                        path: "playground",
                        visiblePath: "playground",
                        code,
                    },
                ];

                const dependencies = props.options.dependenciesPath
                    ? await fetchDependencies(props.options.dependenciesPath)
                    : null;

                console.log({ dependencies });

                const sourceFiles = [{ path: "playground", code }];

                if (dependencies != null) {
                    sourceFiles.push(...dependencies.sourceFiles);
                }

                const compileResult = compile(sources, dependencies?.interface);

                if (compileResult.diagnostics.length > 0) {
                    const sourceCodeForFile = (file: string) => {
                        const sourceFile = sourceFiles.find(({ path }) => path === file);
                        return sourceFile?.code ?? "";
                    };

                    const renderedDiagnostics = renderDiagnostics(
                        compileResult.diagnostics,
                        compileResult.interface,
                        compileResult.library,
                        sourceCodeForFile,
                    );

                    setDiagnostics(renderedDiagnostics);
                }

                const linkResult = link([
                    compileResult.library,
                    ...(dependencies?.libraries ?? []),
                ]);

                if (linkResult.Err) {
                    throw new Error(`failed to link libraries: ${linkResult.Err}`);
                }

                const executable = linkResult.Ok;

                const handleIo = async (request: IoRequest) => {
                    switch (request.type) {
                        case "display": {
                            appendToOutput({ type: "text", text: request.message });
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
                            throw new Error("TODO");
                        }
                        case "sleep": {
                            setTimeout(request.completion, request.ms);
                            break;
                        }
                    }
                };

                clearOutput();

                try {
                    await evaluate(executable, {
                        debug: false,
                        io: handleIo,
                    });
                } catch (error) {
                    if (error instanceof InterpreterError) {
                        // TODO: Show output
                    } else {
                        throw error;
                    }
                }
            } catch (error) {
                console.error(error);
            }
        })();
    }, [code, props.options]);

    return <pre>{JSON.stringify(output, null, 4)}</pre>;
};

interface FetchDependenciesResult {
    interface: any;
    libraries: any;
    sourceFiles: { path: string; code: string }[];
}

const fetchDependencies = async (name: string): Promise<FetchDependenciesResult> => {
    const baseUrlString = import.meta.env.VITE_DEPENDENCIES_BASE_URL as string;

    if (!baseUrlString) {
        throw new Error("missing VITE_DEPENDENCIES_BASE_URL");
    }

    return fetch(new URL(`${name}.json`, baseUrlString)).then((response) => response.json());
};
