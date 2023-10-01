import { useRefState } from "./ref-state";
import { AnalysisOutput, AnalysisOutputDiagnostic, HoverOutput, useRunner } from "./runner";
import { useMemo, useState, useEffect, forwardRef, useImperativeHandle } from "react";
import { useSpring, animated } from "react-spring";
import {
    Button,
    debounce,
    FormControl,
    IconButton,
    InputAdornment,
    InputLabel,
    MenuItem,
    Select,
    TextField,
} from "@mui/material";
import ErrorIcon from "@mui/icons-material/Error";
import Refresh from "@mui/icons-material/Refresh";
import KeyboardReturn from "@mui/icons-material/KeyboardReturn";
import SubjectRounded from "@mui/icons-material/SubjectRounded";
import lineColumn from "line-column";
import useMeasure from "react-use-measure";
import ReactMarkdown from "react-markdown";
import remarkMath from "remark-math";
import remarkGfm from "remark-gfm";
import rehypeKatex from "rehype-katex";
import remarkSmartypants from "remark-smartypants";
import rehypeRaw from "rehype-raw";
import "./styles/globals.css";

export interface Output {
    isEmpty: boolean;
}

interface OutputInternal {
    code: string;
    items: OutputItem[];
    diagnostics: AnalysisOutputDiagnostic[];
}

type OutputItem =
    | { type: "output"; text: string }
    | { type: "prompt"; prompt: string; onSubmit: (text: string) => Promise<boolean> }
    | { type: "choice"; prompt: string; choices: string[]; onSubmit: (index: number) => void }
    | { type: "custom"; id: string };

interface UiElement {
    onMessage: Record<string, (message: string, value: any) => Promise<any>>;
    cleanup: () => Promise<void>;
}

export interface PlaygroundRunner {
    isRunning: () => boolean;
    format: () => Promise<string | undefined>;
    hover: (start: number, end: number) => Promise<HoverOutput | null>;
    clearOutput: () => void;
}

export const PlaygroundRunner = forwardRef<
    PlaygroundRunner,
    {
        id: string;
        code: string;
        outputClassName?: string;
        beginner: boolean;
        lint: boolean;
        setup: string | undefined;
        autoRun: boolean;
        containsTemplates: () => boolean;
        onReset?: () => void;
        onAnalyze?: (analysis: AnalysisOutput) => void;
        onChangeOutput?: (output: Output) => void;
        onError?: (error: any) => void;
        footer?: () => JSX.Element;
    }
>((props, ref) => {
    const runner = useRunner({ id: props.id, code: props.code });

    const [isRunning, setRunning] = useRefState(false);
    const [output, setOutput] = useRefState<OutputInternal | undefined>(undefined);
    const appendToOutput = (code: string, item: OutputItem) =>
        setOutput((output) =>
            output
                ? { code, items: [...output.items, item], diagnostics: output.diagnostics }
                : { code, items: [item], diagnostics: [] }
        );

    useEffect(() => {
        props.onChangeOutput?.({
            isEmpty: (output.current?.items.length ?? 0) === 0,
        });
    }, [output]);

    const [fatalError, setFatalError] = useState(false);

    const [uiElements, setUiElements] = useRefState<UiElement[]>([]);
    const [currentUiElementId, setCurrentUiElementId] = useRefState("");

    const [showTemplatesWarning, setShowTemplatesWarning] = useState(false);

    const [outputRef, { height: outputHeight }] = useMeasure();

    const animatedOutputStyle = useSpring(
        fatalError || output.current != null
            ? { opacity: 1, height: outputHeight }
            : { opacity: 0, height: 0 }
    );

    const run = useMemo(
        () =>
            debounce(async (code: string, lint: boolean, setup: string | undefined) => {
                try {
                    props.onReset?.();

                    setRunning(true);
                    setFatalError(false);

                    const analysis = await runner.analyze(code, lint, setup);

                    const diagnostics = props.containsTemplates() ? [] : analysis.diagnostics;

                    props.onAnalyze?.({ ...analysis, diagnostics });

                    setOutput({
                        code,
                        items: [],
                        diagnostics,
                    });

                    for (const el of uiElements.current) {
                        await el.cleanup();
                    }

                    setUiElements([]);

                    setShowTemplatesWarning(props.containsTemplates());

                    if (!analysis.diagnostics.find(({ level }) => level === "error")) {
                        await runner.run(async (request) => {
                            try {
                                switch (request.type) {
                                    case "display":
                                        appendToOutput(code, {
                                            type: "output",
                                            text: request.text,
                                        });

                                        request.callback();

                                        break;
                                    case "prompt":
                                        appendToOutput(code, {
                                            type: "prompt",
                                            prompt: request.prompt,
                                            onSubmit: async (text) => {
                                                request.sendInput(text);

                                                const valid = await request.recvValid();
                                                if (valid) {
                                                    request.callback();
                                                }

                                                return valid;
                                            },
                                        });

                                        break;
                                    case "choice":
                                        appendToOutput(code, {
                                            type: "choice",
                                            prompt: request.prompt,
                                            choices: request.choices,
                                            onSubmit: request.callback,
                                        });

                                        break;
                                    case "loadUi": {
                                        const uiElement = await import(
                                            /* @vite-ignore */ request.url
                                        );

                                        const index = uiElements.current.length;

                                        const id = `${props.id}-${index}`;
                                        setCurrentUiElementId(id);
                                        appendToOutput(code, { type: "custom", id });

                                        requestAnimationFrame(() => {
                                            requestAnimationFrame(async () => {
                                                const container = document.getElementById(
                                                    id
                                                )! as HTMLDivElement;

                                                if (!container) {
                                                    throw new Error("container not initialized");
                                                }

                                                await uiElement.initialize(id, container);

                                                setUiElements([
                                                    ...uiElements.current,
                                                    {
                                                        onMessage: uiElement.onMessage,
                                                        cleanup: () => uiElement.cleanup?.(id),
                                                    },
                                                ]);

                                                request.callback();
                                            });
                                        });

                                        break;
                                    }
                                    case "messageUi": {
                                        const id = currentUiElementId.current.split("-");
                                        const index = parseInt(id[id.length - 1]);
                                        const uiElement = uiElements.current[index];

                                        if (!uiElement) {
                                            throw new Error(`invalid UI element ${index}`);
                                        }

                                        const onMessage =
                                            uiElement.onMessage[currentUiElementId.current];

                                        if (!onMessage) {
                                            console.warn(
                                                `sent message to UI element ${index} after calling 'cleanup'`
                                            );
                                            break;
                                        }

                                        const result = await onMessage(
                                            request.message,
                                            request.value
                                        );

                                        request.callback(result);

                                        break;
                                    }
                                    default:
                                        console.error(
                                            `[code editor ${props.id}] unknown request:`,
                                            request
                                        );

                                        break;
                                }
                            } catch (error) {
                                props.onError?.(error);

                                console.error(`[code editor ${props.id}] error:`, error);
                            }
                        });
                    }
                } catch (error) {
                    setFatalError(true);
                } finally {
                    setRunning(false);
                }
            }, 500),
        [props.id, props.onError]
    );

    useEffect(() => {
        if (props.autoRun) {
            run(props.code, props.lint, props.setup);
        }
    }, [props.code, props.lint, props.setup, props.autoRun]);

    useImperativeHandle(ref, () => ({
        isRunning: () => isRunning.current,
        format: () => runner.format(props.code),
        hover: runner.hover,
        clearOutput: () => setOutput({ code: props.code, items: [], diagnostics: [] }),
    }));

    return (
        <animated.div style={animatedOutputStyle}>
            <div ref={outputRef}>
                {(() => {
                    if (fatalError) {
                        return (
                            <div className="flex flex-col gap-4 p-6 text-red-500 bg-red-50 dark:bg-red-900 dark:bg-opacity-20">
                                <div className="flex items-center gap-2">
                                    <ErrorIcon fontSize="large" />
                                    <h1 className="text-xl">Internal Error</h1>
                                </div>

                                <p className="text-gray-500 dark:text-gray-400">
                                    Wipple encountered an internal error while running your code.
                                    Please reload the page and try again.
                                </p>
                            </div>
                        );
                    }

                    if (showTemplatesWarning) {
                        return (
                            <div className="flex flex-col gap-4 p-6 text-blue-500 bg-blue-50 dark:bg-blue-900 dark:bg-opacity-20">
                                <div className="flex items-center gap-2">
                                    <SubjectRounded fontSize="large" />
                                    <h1 className="text-xl">Placeholders in Code</h1>
                                </div>

                                <p className="text-gray-500 dark:text-gray-400">
                                    Your code contains placeholders that need to be filled before
                                    your program can be run.
                                </p>
                            </div>
                        );
                    }

                    if (output.current == null) {
                        return null;
                    }

                    return (
                        <div>
                            {output.current.diagnostics.length ? (
                                props.beginner ?? true ? (
                                    output.current.diagnostics.find(
                                        ({ level }) => level === "error"
                                    ) ? (
                                        <div className="flex flex-col gap-4 p-6 text-red-500 bg-red-50 dark:bg-red-900 dark:bg-opacity-20">
                                            <div className="flex items-center gap-2">
                                                <ErrorIcon fontSize="large" />
                                                <h1 className="text-xl">Error</h1>
                                            </div>

                                            <p className="text-gray-500 dark:text-gray-400">
                                                Wipple couldn’t run your code because it has errors.
                                            </p>
                                        </div>
                                    ) : null
                                ) : (
                                    <div
                                        className={`p-4 flex flex-col gap-4 text-gray-900 dark:text-gray-50 ${
                                            output.current.diagnostics.find(
                                                ({ level }) => level === "error"
                                            )
                                                ? "diagnostic-errors"
                                                : "diagnostic-warnings"
                                        }`}
                                    >
                                        {output.current.diagnostics.map((diagnostic, index) => (
                                            <div
                                                key={index}
                                                className="flex gap-2 overflow-x-scroll"
                                            >
                                                <div
                                                    className={`rounded-sm border-r-4 ${
                                                        diagnostic.level === "error"
                                                            ? "border-r-red-500"
                                                            : "border-r-yellow-500"
                                                    }`}
                                                />

                                                <div key={index} className="flex flex-col gap-2.5">
                                                    <div
                                                        className={`font-bold ${
                                                            diagnostic.level === "error"
                                                                ? "text-red-600 dark:text-red-500"
                                                                : "text-yellow-600 dark:text-yellow-500"
                                                        }`}
                                                    >
                                                        <Markdown>
                                                            {`${diagnostic.level}: ${diagnostic.message}`}
                                                        </Markdown>
                                                    </div>

                                                    {diagnostic.notes.map((note, noteIndex) => {
                                                        const lookup = lineColumn(
                                                            note.code + "\n\n"
                                                        );

                                                        let result = lookup.fromIndex(
                                                            note.span.start
                                                        );

                                                        if (!result) {
                                                            return null;
                                                        }

                                                        const { line, col } = result;

                                                        const start = lookup.toIndex(line, 1);

                                                        const end = lookup.toIndex(line + 1, 1);

                                                        return (
                                                            <div
                                                                className="flex flex-col"
                                                                key={noteIndex}
                                                            >
                                                                <div>
                                                                    <p className="semibold text-xs opacity-50">
                                                                        {note.span.file ===
                                                                        "playground"
                                                                            ? ""
                                                                            : `${note.span.file}, `}
                                                                        line {line}
                                                                    </p>

                                                                    <pre>
                                                                        <span>
                                                                            {note.code.slice(
                                                                                start,
                                                                                note.span.start
                                                                            )}
                                                                        </span>

                                                                        <span
                                                                            className={`rounded-sm underline underline-offset-4 ${
                                                                                diagnostic.level ===
                                                                                "error"
                                                                                    ? "text-red-600 bg-red-100 dark:text-red-500 dark:bg-red-900 dark:bg-opacity-50"
                                                                                    : "text-yellow-600 bg-yellow-100 dark:text-yellow-500 dark:bg-yellow-800 dark:bg-opacity-50"
                                                                            }`}
                                                                        >
                                                                            {note.code.slice(
                                                                                note.span.start,
                                                                                note.span.end
                                                                            )}
                                                                        </span>

                                                                        <span>
                                                                            {note.code.slice(
                                                                                note.span.end,
                                                                                end
                                                                            )}
                                                                        </span>
                                                                    </pre>
                                                                </div>

                                                                {note.messages.map(
                                                                    (message, messageIndex) => (
                                                                        <div
                                                                            key={messageIndex}
                                                                            className={`flex ${
                                                                                noteIndex === 0 &&
                                                                                messageIndex === 0
                                                                                    ? diagnostic.level ===
                                                                                      "error"
                                                                                        ? "text-red-600 dark:text-red-500"
                                                                                        : "text-yellow-600 dark:text-yellow-500"
                                                                                    : "opacity-75"
                                                                            }`}
                                                                        >
                                                                            <pre>
                                                                                {new Array(col - 1)
                                                                                    .fill(" ")
                                                                                    .join("")}
                                                                            </pre>

                                                                            <Markdown>
                                                                                {message}
                                                                            </Markdown>
                                                                        </div>
                                                                    )
                                                                )}

                                                                {noteIndex === 0 &&
                                                                diagnostic.example ? (
                                                                    <div className="flex">
                                                                        <pre>
                                                                            {new Array(col - 1)
                                                                                .fill(" ")
                                                                                .join("")}
                                                                        </pre>
                                                                        <span className="opacity-75">
                                                                            for more information,
                                                                            see{"\u00A0"}
                                                                        </span>
                                                                        <a
                                                                            target="_blank"
                                                                            className="text-sky-500"
                                                                            href={`/playground/?lesson=errors/${diagnostic.example}`}
                                                                        >
                                                                            this guide
                                                                        </a>
                                                                    </div>
                                                                ) : null}
                                                            </div>
                                                        );
                                                    })}
                                                </div>
                                            </div>
                                        ))}
                                    </div>
                                )
                            ) : null}

                            {output.current.items.length ? (
                                <div
                                    className={`p-4 ${
                                        props.outputClassName ?? ""
                                    } text-black dark:text-white`}
                                >
                                    {output.current.items.map((item, index) => {
                                        switch (item.type) {
                                            case "output":
                                                return (
                                                    <Markdown
                                                        key={index}
                                                        className="prose prose-sky dark:prose-invert max-w-none"
                                                    >
                                                        {item.text}
                                                    </Markdown>
                                                );
                                            case "prompt":
                                                return (
                                                    <InputField
                                                        key={index}
                                                        index={index}
                                                        onSubmit={item.onSubmit}
                                                    >
                                                        {item.prompt}
                                                    </InputField>
                                                );
                                            case "choice":
                                                return (
                                                    <DropdownField
                                                        key={index}
                                                        index={index}
                                                        choices={item.choices}
                                                        onSubmit={item.onSubmit}
                                                    >
                                                        {item.prompt}
                                                    </DropdownField>
                                                );
                                            case "custom":
                                                return <div key={index} id={item.id} />;
                                        }
                                    })}

                                    {isRunning.current ? (
                                        <div className="bouncing-loader">
                                            <div />
                                            <div />
                                            <div />
                                        </div>
                                    ) : output.current.items.find(
                                          (item) => item.type !== "output"
                                      ) || uiElements.current.length ? (
                                        <div className="mt-4">
                                            <Button
                                                variant="contained"
                                                size="small"
                                                endIcon={<Refresh />}
                                                onClick={() =>
                                                    run(props.code, props.lint, props.setup)
                                                }
                                            >
                                                Run again
                                            </Button>
                                        </div>
                                    ) : null}
                                </div>
                            ) : null}
                        </div>
                    );
                })()}

                {props.footer && <props.footer />}
            </div>
        </animated.div>
    );
});

const InputField = (props: {
    index: number;
    onSubmit: (text: string) => Promise<boolean>;
    children: string;
}) => {
    const [text, setText] = useState("");
    const [isEnabled, setEnabled] = useState(true);
    const [isValid, setValid] = useState(true);

    const submit = async () => {
        const valid = await props.onSubmit(text);
        setValid(valid);
        if (valid) {
            setEnabled(false);
        }
    };

    return (
        <form
            className={props.index === 0 ? "mb-4" : "my-4"}
            onSubmit={(e) => {
                e.preventDefault();
                submit();
            }}
        >
            <TextField
                label={props.children}
                variant="outlined"
                fullWidth
                value={text}
                disabled={!isEnabled}
                error={!isValid}
                onChange={(event) => setText(event.target.value)}
                InputProps={{
                    endAdornment: (
                        <InputAdornment position="end">
                            <IconButton disabled={!isEnabled} onClick={submit}>
                                <KeyboardReturn />
                            </IconButton>
                        </InputAdornment>
                    ),
                }}
            />
        </form>
    );
};

const DropdownField = (props: {
    index: number;
    choices: string[];
    onSubmit: (index: number) => void;
    children: string;
}) => {
    const [index, setIndex] = useState<number | undefined>();
    const [isEnabled, setEnabled] = useState(true);

    return (
        <div className={`flex flex-row gap-4 ${props.index !== 0 ? "mb-4" : "my-4"}`}>
            <FormControl className="flex-grow">
                <InputLabel>{props.children}</InputLabel>

                <Select
                    label={props.children}
                    disabled={!isEnabled}
                    value={index ?? ""}
                    onChange={(event) => setIndex(event.target.value as number)}
                >
                    {props.choices.map((choice, index) => (
                        <MenuItem key={index} value={index}>
                            {choice}
                        </MenuItem>
                    ))}
                </Select>
            </FormControl>

            <Button
                disabled={!isEnabled || index == null}
                onClick={() => {
                    props.onSubmit(index!);
                    setEnabled(false);
                }}
            >
                Submit
            </Button>
        </div>
    );
};

export const Markdown = (props: { children: string; className?: string }) => (
    <span className={"code-editor-markdown " + props.className ?? ""}>
        <ReactMarkdown
            remarkPlugins={[remarkMath, remarkGfm, remarkSmartypants]}
            rehypePlugins={[rehypeRaw, rehypeKatex]}
            linkTarget="_blank"
        >
            {props.children}
        </ReactMarkdown>
    </span>
);

export * from "./runner";

export { useRefState };
