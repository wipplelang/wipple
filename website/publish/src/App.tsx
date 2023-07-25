import { useEffect, useRef, useState } from "react";
import * as glue from "../../../tools/playground-runner/clientGlue";
import { Output, OutputItem, OutputMethods, useRefState } from "../common";
import Runner from "../../../tools/playground-runner/worker?worker";

export const App = (props: { program: any }) => {
    const [output, setOutput] = useRefState<
        { items: OutputItem[]; diagnostics: never[] } | undefined
    >(undefined);
    const [fatalError, setFatalError] = useState(false);

    const appendToOutput = (item: OutputItem) =>
        setOutput((output) =>
            output
                ? { items: [...output.items, item], diagnostics: output.diagnostics }
                : { items: [item], diagnostics: [] }
        );

    const [isRunning, setRunning] = useState(false);

    const outputRef = useRef<OutputMethods>(null);

    const runner = useRunner(props.program);

    const run = async () => {
        try {
            setRunning(true);
            setOutput(undefined);

            await runner.waitForLoad();
            await outputRef.current!.run();
        } catch (error) {
            setFatalError(true);
        } finally {
            setRunning(false);
        }
    };

    useEffect(() => {
        run();
    }, []);

    return (
        <div>
            <Output
                ref={outputRef}
                id="main"
                isRunning={isRunning}
                firstLayout={false}
                onLayout={() => {}}
                showTemplatesWarning={false}
                run={runner.run}
                output={output.current}
                onAddOutputItem={appendToOutput}
                fatalError={fatalError}
                onFatalError={() => setFatalError(true)}
                beginner={false}
                onRefresh={run}
            />
        </div>
    );
};

const useRunner = (program: any) => {
    const runner = useRef<Worker | null>(null);

    const newRunnerPromise = () =>
        new Promise<void>((resolve, reject) => {
            runner.current = new Runner();

            runner.current.onmessage = (event) => {
                switch (event.data.type) {
                    case "loaded":
                        console.log("runner loaded");
                        runner.current!.onmessage = null;
                        resolve();
                        setLoaded(true);
                        break;
                    default:
                        runner.current!.onmessage = null;
                        reject(new Error("invalid operation"));
                        break;
                }
            };

            runner.current!.postMessage({ operation: "checkLoading" });
        });

    const runnerPromise = useRef<Promise<void>>();
    const [loaded, setLoaded] = useState(false);

    const reset = () => {
        runner.current?.terminate();
        runner.current = null;

        setLoaded(false);
        runnerPromise.current = newRunnerPromise();
    };

    useEffect(() => {
        reset();
    }, []);

    return {
        isLoaded: loaded,
        waitForLoad: () => runnerPromise.current!,
        run: (handleConsole: (request: glue.ConsoleRequest) => void) =>
            glue.run({
                loadRunner: async () => {
                    await runnerPromise.current;
                    return runner.current!;
                },
                runArgs: { program },
                handleConsole,
                reset,
            }),
    };
};
