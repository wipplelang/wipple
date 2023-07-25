import ErrorIcon from "@mui/icons-material/Error";
import type { AnalysisOutputDiagnostic } from "./types";
import lineColumn from "line-column";
import {
    Button,
    FormControl,
    IconButton,
    InputAdornment,
    InputLabel,
    MenuItem,
    Select,
    TextField,
} from "@mui/material";
import KeyboardReturn from "@mui/icons-material/KeyboardReturn";
import SubjectRounded from "@mui/icons-material/SubjectRounded";
import Refresh from "@mui/icons-material/Refresh";
import React, { useEffect, useImperativeHandle, useState } from "react";
import { useRefState } from "./ref-state";
import useMeasure from "react-use-measure";
import { useSpring, animated } from "react-spring";
import { ConsoleRequest } from "../../../tools/playground-runner/clientGlue";
import ReactMarkdown from "react-markdown";
import remarkMath from "remark-math";
import remarkGfm from "remark-gfm";
import rehypeKatex from "rehype-katex";
import remarkSmartypants from "remark-smartypants";
import rehypeRaw from "rehype-raw";

export type OutputItem =
    | { type: "output"; text: string }
    | { type: "prompt"; prompt: string; onSubmit: (text: string) => Promise<boolean> }
    | { type: "choice"; prompt: string; choices: string[]; onSubmit: (index: number) => void }
    | { type: "custom"; id: string };

export interface UiElement {
    id: string;
    onMessage: Record<string, (message: string, value: any) => Promise<any>>;
}

export const run = async (options: {
    runGlue: (handleRequest: (request: ConsoleRequest) => void) => Promise<void>;
    appendToOutput: (item: OutputItem) => void;
    currentUiElement: () => UiElement | undefined;
    getNextUiElementId: () => string;
    appendUiElement: (element: UiElement) => void;
    captureException?: (error: any) => void;
}) => {
    await options.runGlue(async (request) => {
        try {
            switch (request.type) {
                case "display":
                    options.appendToOutput({
                        type: "output",
                        text: request.text,
                    });

                    request.callback();

                    break;
                case "prompt":
                    options.appendToOutput({
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
                    options.appendToOutput({
                        type: "choice",
                        prompt: request.prompt,
                        choices: request.choices,
                        onSubmit: request.callback,
                    });

                    break;
                case "loadUi": {
                    const uiElement = await import(/* @vite-ignore */ request.url);

                    const id = options.getNextUiElementId();
                    options.appendToOutput({ type: "custom", id });

                    requestAnimationFrame(() => {
                        requestAnimationFrame(async () => {
                            const container = document.getElementById(id)! as HTMLDivElement;

                            if (!container) {
                                throw new Error("container not initialized");
                            }

                            await uiElement.initialize(id, container);

                            options.appendUiElement({ id, onMessage: uiElement.onMessage });

                            requestAnimationFrame(request.callback);
                        });
                    });

                    break;
                }
                case "messageUi": {
                    const uiElement = options.currentUiElement();

                    if (!uiElement) {
                        throw new Error("invalid UI element");
                    }

                    const result = await uiElement.onMessage[uiElement.id](
                        request.message,
                        request.value
                    );

                    request.callback(result);

                    break;
                }
                default:
                    console.error("[code editor] unknown request:", request);

                    break;
            }
        } catch (error) {
            options.captureException?.(error);
            console.error("[code editor] error:", error);
        }
    });
};

export interface OutputMethods {
    run: () => Promise<void>;
}

export const Output = React.forwardRef<
    OutputMethods,
    {
        id: string;
        isRunning: boolean;
        firstLayout: boolean;
        showTemplatesWarning: boolean;
        onLayout: () => void;
        run: (handleConsole: (request: ConsoleRequest) => void) => Promise<void>;
        output: { items: OutputItem[]; diagnostics: AnalysisOutputDiagnostic[] } | undefined;
        onAddOutputItem: (item: OutputItem) => void;
        fatalError: boolean;
        onFatalError: () => void;
        captureException?: (error: any) => void;
        beginner: boolean;
        onRefresh: () => void;
    }
>((props, ref) => {
    const [outputRef, { height: outputHeight }] = useMeasure();

    const animatedOutputStyleDefaults = {
        immediate: props.firstLayout,
        onRest: props.onLayout,
    };

    const animatedOutputStyle = useSpring(
        props.isRunning || props.fatalError || props.output != null
            ? { ...animatedOutputStyleDefaults, opacity: 1, height: outputHeight }
            : { ...animatedOutputStyleDefaults, opacity: 0, height: 0 }
    );

    const [uiElements, setUiElements] = useRefState<UiElement[]>([]);
    const [currentUiElementId, setCurrentUiElementId] = useRefState("");

    useImperativeHandle(
        ref,
        () => ({
            run: async () => {
                setUiElements([]);

                try {
                    await run({
                        runGlue: props.run,
                        appendToOutput: props.onAddOutputItem,
                        currentUiElement: () => {
                            const id = currentUiElementId.current.split("-");
                            const index = parseInt(id[id.length - 1]);
                            return uiElements.current[index];
                        },
                        getNextUiElementId: () => {
                            const index = uiElements.current.length;

                            const id = `${props.id}-${index}`;
                            setCurrentUiElementId(id);

                            return id;
                        },
                        appendUiElement: (element) =>
                            setUiElements([...uiElements.current, element]),
                    });
                } catch (error) {
                    console.error(error);
                    props.onFatalError();
                }
            },
        }),
        [props.run, props.onAddOutputItem, props.onFatalError]
    );

    return (
        <animated.div style={animatedOutputStyle}>
            <div ref={outputRef}>
                {(() => {
                    if (props.fatalError) {
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

                    if (props.showTemplatesWarning) {
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

                    if (props.output == null && !props.isRunning) {
                        return null;
                    }

                    return (
                        <div>
                            {props.output?.diagnostics.length ? (
                                props.beginner ? (
                                    props.output.diagnostics.find(
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
                                            props.output.diagnostics.find(
                                                ({ level }) => level === "error"
                                            )
                                                ? "diagnostic-errors"
                                                : "diagnostic-warnings"
                                        }`}
                                    >
                                        {props.output.diagnostics.map((diagnostic, index) => (
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

                                                        let { line, col } = lookup.fromIndex(
                                                            note.span.start
                                                        )!;

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
                                                            </div>
                                                        );
                                                    })}
                                                </div>
                                            </div>
                                        ))}
                                    </div>
                                )
                            ) : null}

                            {props.isRunning || props.output?.items.length ? (
                                <div className="p-4 bg-gray-50 dark:bg-gray-800 text-black dark:text-white">
                                    {props.output?.items.map((item, index) => {
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

                                    {props.isRunning ? (
                                        <div className="bouncing-loader">
                                            <div />
                                            <div />
                                            <div />
                                        </div>
                                    ) : props.output?.items.find(
                                          (item) => item.type !== "output"
                                      ) || uiElements.current.length ? (
                                        <div className="mt-4">
                                            <Button
                                                variant="contained"
                                                size="small"
                                                endIcon={<Refresh />}
                                                onClick={props.onRefresh}
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
    <div className={"code-editor-markdown " + props.className ?? ""}>
        <ReactMarkdown
            remarkPlugins={[remarkMath, remarkGfm, remarkSmartypants]}
            rehypePlugins={[rehypeRaw, rehypeKatex]}
            linkTarget="_blank"
        >
            {props.children}
        </ReactMarkdown>
    </div>
);
