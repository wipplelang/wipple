import { useRefState } from "./ref-state";
import {
    AnalysisConsoleDiagnosticFix,
    AnalysisOutput,
    AnalysisOutputDiagnostic,
    HoverOutput,
    useRunner,
} from "./runner";
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
import ExpandMoreIcon from "@mui/icons-material/ExpandMore";
import ExpandLessIcon from "@mui/icons-material/ExpandLess";
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
import { produce } from "immer";

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
    format: (code: string) => Promise<string | undefined>;
    completion: (prefix: string) => Promise<string | undefined>;
    hover: (start: number, end: number, nameOnly: boolean) => Promise<HoverOutput | null>;
    clearOutput: () => void;
    expandSnippet: (
        snippet: { file: any; counter: number },
        wrappedCode: string | null
    ) => Promise<string | null>;
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
        onApplyFix: (fix: AnalysisConsoleDiagnosticFix) => void;
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
                        items:
                            analysis.diagnostics.filter((d) => d.level === "error").length > 0
                                ? output.current?.items ?? []
                                : [],
                        diagnostics,
                    });

                    if (analysis.diagnostics.length === 0) {
                        for (const el of uiElements.current) {
                            await el.cleanup();
                        }

                        setUiElements([]);
                    }

                    setOpenOutputs(new Array(analysis.diagnostics.length).fill(false));

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
        format: runner.format,
        completion: runner.completion,
        hover: runner.hover,
        clearOutput: () => setOutput({ code: props.code, items: [], diagnostics: [] }),
        expandSnippet: runner.expandSnippet,
    }));

    const [openOutputs, setOpenOutputs] = useState<boolean[]>([]);

    return (
        <animated.div style={animatedOutputStyle}>
            <div className={props.outputClassName} ref={outputRef}>
                {(() => {
                    if (fatalError) {
                        return (
                            <div className="flex flex-col gap-4 p-6 text-red-500">
                                <div className="flex items-center gap-2">
                                    <ErrorIcon />
                                    <h1 className="text-lg">Internal Error</h1>
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
                            <div className="flex flex-col gap-4 p-6 text-sky-500">
                                <div className="flex items-center gap-2">
                                    <SubjectRounded />
                                    <h1 className="text-lg">Placeholders in Code</h1>
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
                                <div className="p-4 flex flex-col gap-4 text-gray-900 dark:text-gray-50">
                                    {output.current.diagnostics.map((diagnostic, index) => (
                                        <div
                                            key={index}
                                            className="flex gap-2 overflow-x-scroll bg-white dark:bg-gray-900 p-4 rounded-lg shadow-lg shadow-gray-100 dark:shadow-gray-900"
                                        >
                                            <div
                                                className={`rounded-sm border-r-4 ${
                                                    diagnostic.level === "error"
                                                        ? "border-r-sky-500"
                                                        : "border-r-yellow-500"
                                                }`}
                                            />

                                            <div key={index} className="flex flex-col gap-2.5">
                                                <div className="flex flex-col items-start gap-1">
                                                    <DiagnosticLine
                                                        primary
                                                        level={diagnostic.level}
                                                        span={diagnostic.span}
                                                        code={diagnostic.code}
                                                        messages={[diagnostic.message]}
                                                    />

                                                    <div className="flex gap-2">
                                                        {diagnostic.fix && (
                                                            <div className="flex">
                                                                <button
                                                                    className="px-2 py-1 rounded-lg bg-sky-500 text-white"
                                                                    onClick={() => {
                                                                        props.onApplyFix(
                                                                            diagnostic.fix!
                                                                        );
                                                                    }}
                                                                >
                                                                    <Markdown>
                                                                        {diagnostic.fix.description}
                                                                    </Markdown>
                                                                </button>
                                                            </div>
                                                        )}

                                                        {diagnostic.example && (
                                                            <a
                                                                target="_blank"
                                                                href={`/playground/?lesson=errors/${diagnostic.example}`}
                                                                className={`flex gap-1 px-2 py-1 rounded-lg bg-opacity-10 ${
                                                                    diagnostic.level === "error"
                                                                        ? "bg-sky-500 text-sky-500"
                                                                        : "bg-yellow-500 text-yellow-500"
                                                                }`}
                                                            >
                                                                Help
                                                            </a>
                                                        )}

                                                        {diagnostic.notes.length > 0 ? (
                                                            <button
                                                                className={`flex gap-1 pl-2 pr-1 py-1 rounded-lg bg-opacity-10 ${
                                                                    diagnostic.level === "error"
                                                                        ? "bg-sky-500 text-sky-500"
                                                                        : "bg-yellow-500 text-yellow-500"
                                                                }`}
                                                                onClick={() => {
                                                                    setOpenOutputs(
                                                                        produce((openOutputs) => {
                                                                            openOutputs[index] =
                                                                                !openOutputs[index];
                                                                        })
                                                                    );
                                                                }}
                                                            >
                                                                {openOutputs[index] ? (
                                                                    <>
                                                                        Show less
                                                                        <ExpandLessIcon />
                                                                    </>
                                                                ) : (
                                                                    <>
                                                                        Show more <ExpandMoreIcon />
                                                                    </>
                                                                )}
                                                            </button>
                                                        ) : null}
                                                    </div>
                                                </div>

                                                {openOutputs[index]
                                                    ? diagnostic.notes.map((note, noteIndex) => (
                                                          <DiagnosticLine
                                                              key={noteIndex}
                                                              level={diagnostic.level}
                                                              span={note.span}
                                                              code={note.code}
                                                              messages={note.messages}
                                                          />
                                                      ))
                                                    : null}
                                            </div>
                                        </div>
                                    ))}
                                </div>
                            ) : null}

                            {output.current.items.length ? (
                                <div className="p-4 text-black dark:text-white">
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

const DiagnosticLine = (props: {
    primary?: true;
    level: "warning" | "error";
    span: AnalysisOutputDiagnostic["span"];
    code: string;
    messages: string[];
}) => {
    const lookup = lineColumn(props.code + "\n\n");

    let result = lookup.fromIndex(props.span.start);

    if (!result) {
        return null;
    }

    const { line, col } = result;

    const start = lookup.toIndex(line, 1);

    const end = lookup.toIndex(line + 1, 1);

    return (
        <div className="flex flex-col">
            <div>
                <p className="semibold text-xs opacity-50">
                    {props.span.file ? `${props.span.file}, ` : ""}
                    line {line}
                </p>

                <pre>
                    <span>{props.code.slice(start, props.span.start)}</span>

                    <span
                        className={`rounded-sm underline decoration-wavy underline-offset-4 ${
                            props.level === "error" ? "decoration-sky-500" : "decoration-yellow-500"
                        }`}
                    >
                        {props.code.slice(props.span.start, props.span.end)}
                    </span>

                    <span>{props.code.slice(props.span.end, end)}</span>
                </pre>
            </div>

            {props.messages.map((message, messageIndex) => (
                <div
                    key={messageIndex}
                    className={`flex ${
                        props.primary && messageIndex === 0
                            ? props.level === "error"
                                ? "text-sky-500"
                                : "text-yellow-500"
                            : "opacity-75"
                    }`}
                >
                    <pre>{new Array(col - 1).fill(" ").join("")}</pre>

                    <Markdown>{message}</Markdown>
                </div>
            ))}
        </div>
    );
};

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
