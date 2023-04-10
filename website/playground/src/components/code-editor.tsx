import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import SimpleCodeEditor from "react-simple-code-editor";
import * as prism from "prismjs";
import {
    Button,
    debounce,
    Divider,
    FormControl,
    IconButton,
    InputAdornment,
    InputLabel,
    ListItemText,
    ListSubheader,
    Menu,
    MenuItem,
    MenuList,
    Select,
    TextField,
    useMediaQuery,
} from "@mui/material";
import { Globals as SpringGlobals, useSpring, animated } from "react-spring";
import useMeasure from "react-use-measure";
import ReactMarkdown from "react-markdown";
import remarkMath from "remark-math";
import remarkGfm from "remark-gfm";
import rehypeKatex from "rehype-katex";
import remarkSmartypants from "remark-smartypants";
import rehypeRaw from "rehype-raw";
import {
    AnalysisOutputDiagnostic,
    AnalysisOutputSyntaxHighlightingItem,
    HoverOutput,
    AnalysisOutputCompletions,
    Completion,
    useRunner,
} from "../../runner";
import KeyboardReturn from "@mui/icons-material/KeyboardReturn";
import Refresh from "@mui/icons-material/Refresh";
import Add from "@mui/icons-material/Add";
import getCaretCoordinates from "textarea-caret";
import lineColumn from "line-column";
import { useRefState } from "../helpers";
import { Settings } from "../App";

export interface CodeEditorProps {
    id: string;
    code: string;
    lint: boolean;
    settings: Settings;
    autoFocus: boolean;
    onChange: (code: string) => void;
}

type OutputItem =
    | { type: "output"; text: string }
    | { type: "prompt"; prompt: string; onSubmit: (text: string) => Promise<boolean> }
    | { type: "choice"; prompt: string; choices: string[]; onSubmit: (index: number) => void }
    | { type: "custom"; id: string };

interface Hover {
    x: number;
    y: number;
    output: HoverOutput;
}

interface UiElement {
    onMessage: Record<string, (message: string, value: any) => Promise<any>>;
}

const closingBrackets: Record<string, string> = {
    "(": ")",
    "{": "}",
    "[": "]",
};

const closingCharacters: Record<string, string> = {
    ...closingBrackets,
    '"': '"',
};

export const CodeEditor = (props: CodeEditorProps) => {
    const containerID = `code-editor-container-${props.id}`;
    const editorID = `code-editor-editor-${props.id}`;
    const textAreaID = `code-editor-text-${props.id}`;

    const [syntaxHighlighting, setSyntaxHighlighting] = useState<
        AnalysisOutputSyntaxHighlightingItem[]
    >([]);

    const [isRunning, setRunning] = useState(false);
    const [output, setOutput] = useState<
        { code: string; items: OutputItem[]; diagnostics: AnalysisOutputDiagnostic[] } | undefined
    >();
    const appendToOutput = (code: string, item: OutputItem) =>
        setOutput((output) =>
            output
                ? { code, items: [...output.items, item], diagnostics: output.diagnostics }
                : { code, items: [item], diagnostics: [] }
        );

    const [completions, setCompletions] = useState<AnalysisOutputCompletions>();

    const [outputRef, { height: outputHeight }] = useMeasure();
    const animatedOutputStyle = useSpring(
        output != null ? { opacity: 1, height: outputHeight } : { opacity: 0, height: 0 }
    );

    const prefersReducedMotion = useMediaQuery("(prefers-reduced-motion)");
    useEffect(() => {
        SpringGlobals.assign({
            skipAnimation: prefersReducedMotion,
        });
    }, [prefersReducedMotion]);

    const [uiElements, setUiElements] = useRefState<UiElement[]>([]);
    const [currentUiElementId, setCurrentUiElementId] = useRefState("");

    const runner = useRunner();

    const run = useMemo(
        () =>
            debounce(async (code: string, lint: boolean) => {
                setRunning(true);

                try {
                    setSyntaxHighlighting([]); // FIXME: Prevent flashing
                    const analysis = await runner.analyze(code, lint);
                    setSyntaxHighlighting(analysis.syntaxHighlighting);
                    setOutput({ code: code, items: [], diagnostics: analysis.diagnostics });
                    setCompletions(analysis.completions);
                    setUiElements([]);

                    if (!analysis.diagnostics.find(({ level }) => level === "error")) {
                        const success = await runner.run(async (request) => {
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
                                                    { onMessage: uiElement.onMessage },
                                                ]);

                                                requestAnimationFrame(request.callback);
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

                                        const result = await uiElement.onMessage[
                                            currentUiElementId.current
                                        ](request.message, request.value);

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
                                console.error(`[code editor ${props.id}] error:`, error);
                            }
                        });

                        if (!success) {
                            throw new Error("runner failed");
                        }
                    }
                } catch (error) {
                    console.error(error);

                    setOutput((output) => ({
                        code,
                        type: "error",
                        items: output!.items,
                        diagnostics: [
                            {
                                level: "error",
                                message:
                                    "internal error: There was a problem running your code. Please reload the page and try again.",
                                notes: [
                                    {
                                        code: "",
                                        span: {
                                            file: "playground",
                                            start: 0,
                                            end: 0,
                                        },
                                        messages: [],
                                    },
                                ],
                            },
                        ],
                    }));
                } finally {
                    setRunning(false);
                }
            }, 500),
        [props.id]
    );

    useEffect(() => {
        run(props.code, props.lint);
    }, [run, props.code, props.lint]);

    const [hover, setHover] = useState<Hover>();

    const textEditor = document.getElementById(textAreaID) as HTMLTextAreaElement | null;

    const [mousePosition, setMousePosition] = useState<[number, number]>();

    useEffect(() => {
        const listener = (e: MouseEvent) => {
            setMousePosition([e.x, e.y]);
        };

        window.addEventListener("mousemove", listener);
        window.addEventListener("click", listener);

        return () => {
            window.removeEventListener("mousemove", listener);
            window.removeEventListener("click", listener);
        };
    }, [setMousePosition]);

    useEffect(() => {
        if (!textEditor) return;

        let hoverTimer: NodeJS.Timeout | undefined = undefined;
        let hoverElement: HTMLElement | undefined = undefined;

        const hoverClasses = ["bg-black", "dark:bg-white", "bg-opacity-10", "dark:bg-opacity-10"];

        const handleMouseMove = (e: MouseEvent) => {
            const mouseX = e.x;
            const mouseY = e.y;

            for (const el of document.querySelectorAll<HTMLSpanElement>(`#${editorID} .token`)) {
                const rect = el.getBoundingClientRect();

                if (
                    mouseX >= rect.left &&
                    mouseX < rect.right &&
                    mouseY >= rect.top &&
                    mouseY < rect.bottom
                ) {
                    setHover(undefined);
                    hoverElement?.classList.remove(...hoverClasses);
                    hoverElement = el;
                    break;
                }
            }

            clearTimeout(hoverTimer);
            hoverTimer = setTimeout(async () => {
                if (
                    !hoverElement ||
                    !hoverElement.dataset.wippleStartIndex ||
                    !hoverElement.dataset.wippleEndIndex
                ) {
                    return;
                }

                const output = await runner.hover(
                    parseInt(hoverElement.dataset.wippleStartIndex),
                    parseInt(hoverElement.dataset.wippleEndIndex)
                );

                if (!output) {
                    return;
                }

                hoverElement.classList.add(...hoverClasses);

                setHover({
                    x: hoverElement.getBoundingClientRect().x,
                    y: hoverElement.getBoundingClientRect().bottom + window.scrollY,
                    output,
                });
            }, 250);
        };

        const handleMouseEnter = () => {
            textEditor.addEventListener("mousemove", handleMouseMove);
        };

        const handleMouseLeave = () => {
            setHover(undefined);
            clearTimeout(hoverTimer);
            hoverElement?.classList.remove(...hoverClasses);
            textEditor.removeEventListener("mousemove", handleMouseMove);
        };

        textEditor.addEventListener("mouseenter", handleMouseEnter);
        textEditor.addEventListener("mouseleave", handleMouseLeave);

        return () => {
            textEditor.removeEventListener("mouseenter", handleMouseEnter);
            textEditor.removeEventListener("mouseleave", handleMouseLeave);
        };
    }, [textEditor]);

    const charStack = useRef<string[]>([]);

    useEffect(() => {
        if (!textEditor) return;

        const handleKeyDown = (e: KeyboardEvent) => {
            const keyTyped = e.key;
            const caretPosition = textEditor.selectionStart;

            const lastTypedChar = charStack.current[charStack.current.length - 1] ?? "";

            if (/Meta|Control|Arrow/.test(keyTyped)) {
                charStack.current = [];
                return;
            }

            // Indent if typing after an opening character
            if (keyTyped === "Enter" && lastTypedChar in closingCharacters) {
                e.preventDefault();

                const lookup = lineColumn(textEditor.value + "\n\n");
                const { line } = lookup.fromIndex(caretPosition)!;
                const lineIndex = lookup.toIndex(line, 1);

                let indent = 1;
                for (const s of textEditor.value.slice(lineIndex)) {
                    if (s === "\t") {
                        indent++;
                    } else {
                        break;
                    }
                }

                const newValue =
                    textEditor.value.slice(0, caretPosition) +
                    `\n${"\t".repeat(indent)}\n${"\t".repeat(indent - 1)}` +
                    textEditor.value.slice(caretPosition);

                props.onChange(newValue);

                requestAnimationFrame(() => {
                    textEditor.setSelectionRange(
                        caretPosition + indent + 1,
                        caretPosition + indent + 1
                    );
                });

                charStack.current = [];

                return;
            }

            // Delete the closing character if the opening character was the
            // last one typed and the user presses Backspace
            if (keyTyped === "Backspace" && lastTypedChar in closingCharacters) {
                e.preventDefault();

                const newValue =
                    textEditor.value.slice(0, caretPosition - 1) +
                    textEditor.value.slice(caretPosition + 1);

                props.onChange(newValue);

                requestAnimationFrame(() => {
                    textEditor.setSelectionRange(caretPosition - 1, caretPosition - 1);
                });

                charStack.current.pop();
                return;
            }

            // "Type over" the balanced closing character if the opening
            // character was just typed
            if (closingCharacters[lastTypedChar] === keyTyped) {
                e.preventDefault();

                const newValue =
                    textEditor.value.slice(0, caretPosition) +
                    closingCharacters[lastTypedChar] +
                    textEditor.value.slice(caretPosition + 1);

                props.onChange(newValue);

                requestAnimationFrame(() => {
                    textEditor.setSelectionRange(caretPosition + 1, caretPosition + 1);
                });

                charStack.current.pop();
                return;
            }

            // Balance opening and closing characters
            const closingChar = closingCharacters[keyTyped];
            if (closingChar) {
                e.preventDefault();

                const newValue =
                    textEditor.value.slice(0, caretPosition) +
                    keyTyped +
                    closingChar +
                    textEditor.value.slice(caretPosition);

                props.onChange(newValue);

                requestAnimationFrame(() => {
                    textEditor.setSelectionRange(caretPosition + 1, caretPosition + 1);
                });

                charStack.current.push(keyTyped);
                return;
            }

            if (keyTyped in closingCharacters) {
                charStack.current.push(keyTyped);
            }
        };

        const handleClick = () => {
            charStack.current = [];
        };

        textEditor.addEventListener("keydown", handleKeyDown);
        textEditor.addEventListener("click", handleClick);

        return () => {
            textEditor.removeEventListener("keydown", handleKeyDown);
            textEditor.removeEventListener("click", handleClick);
        };
    }, [textEditor]);

    useEffect(() => {
        const nodes = [
            ...document.querySelectorAll<HTMLSpanElement>(
                `#${editorID} .language-wipple span.token`
            ),
        ];

        const colors: string[][] = [
            ["bg-red-500", "dark:bg-red-400"],
            ["bg-green-500", "dark:bg-green-400"],
            ["bg-blue-500", "dark:bg-blue-400"],
            ["bg-yellow-500", "dark:bg-yellow-400"],
        ];

        const additionalClasses = ["bg-opacity-20", "dark:bg-opacity-20"];

        if (!props.settings.beginner) {
            for (const node of nodes) {
                for (const color of colors) {
                    node.classList.remove(...color);
                }

                node.classList.remove(...additionalClasses);
            }

            return;
        }

        const stack: string[][] = [];
        for (const node of nodes) {
            let color: string[] | undefined;
            if ([...node.innerText].find((c) => closingBrackets[c])) {
                color = colors[stack.length % colors.length];
                stack.push(color);
            } else if (
                [...node.innerText].find((c) => Object.values(closingBrackets).includes(c))
            ) {
                color = stack[stack.length - 1];
                stack.pop();
            } else {
                color = stack[stack.length - 1];
            }

            if (color) {
                node.classList.add(...color);
                node.classList.add(...additionalClasses);
            }
        }
    }, [props.code, props.settings.beginner]);

    useEffect(() => {
        if (!syntaxHighlighting) return;

        const nodes = [
            ...document.querySelectorAll<HTMLSpanElement>(
                `#${editorID} .language-wipple span.token`
            ),
        ];

        let currentNode = nodes.shift();
        if (!currentNode) return;

        let start = 0;

        for (const item of syntaxHighlighting) {
            while (item.start !== start && item.end !== start + currentNode.innerText.length) {
                start += currentNode.innerText.length;
                currentNode = nodes.shift();
                if (!currentNode) return;
            }

            currentNode.classList.add(item.kind);
            currentNode.dataset.wippleStartIndex = item.start.toString();
            currentNode.dataset.wippleEndIndex = item.end.toString();
        }
    }, [syntaxHighlighting]);

    const getCodeEditorCaretPosition = () => {
        const codeEditor = document.getElementById(textAreaID) as HTMLTextAreaElement | null;
        if (!codeEditor) return undefined;

        const coordinates = getCaretCoordinates(codeEditor, codeEditor.selectionStart);
        if (!coordinates) return undefined;

        return {
            x: coordinates.left,
            minY: coordinates.top,
            midY: coordinates.top + coordinates.height / 2,
            maxY: coordinates.top + coordinates.height,
        };
    };

    const [caretPosition, setCaretPosition] =
        useState<ReturnType<typeof getCodeEditorCaretPosition>>();

    const animatedContextMenuTriggerStyle = useSpring(
        caretPosition != null
            ? {
                  opacity: 1,
                  top: caretPosition.midY - 4,
                  right: 8,
                  zIndex: 9999,
                  pointerEvents: "inherit",
              }
            : {
                  opacity: 0,
                  pointerEvents: "none",
              }
    );

    const updateContextMenuTrigger = useCallback(() => {
        requestAnimationFrame(() => {
            const container = document.getElementById(containerID);

            if (!mousePosition || !container) {
                setCaretPosition(undefined);
                return;
            }

            const [mouseX, mouseY] = mousePosition;
            const containerRect = container.getBoundingClientRect();
            if (
                mouseX < containerRect.left ||
                mouseX >= containerRect.right ||
                mouseY < containerRect.top ||
                mouseY >= containerRect.bottom
            ) {
                setCaretPosition(undefined);
                return;
            }

            setCaretPosition(getCodeEditorCaretPosition());
        });
    }, [mousePosition, textEditor]);

    useEffect(() => {
        const textArea = document.getElementById(textAreaID)!;

        const keydownHandler = () => setCaretPosition(getCodeEditorCaretPosition());
        textArea.addEventListener("keydown", keydownHandler);

        return () => {
            textArea.removeEventListener("keydown", keydownHandler);
        };
    }, []);

    useEffect(updateContextMenuTrigger, [props.id, mousePosition]);

    const [contextMenuAnchor, setContextMenuAnchor] = useState<HTMLElement>();

    const showContextMenu = () => {
        if (!caretPosition) {
            console.error("attempt to show context menu without caret position");
            return;
        }

        const anchor = document.createElement("div");
        anchor.style.position = "absolute";
        anchor.style.top = `${caretPosition.maxY + 20}px`;
        anchor.style.left = `${caretPosition.x}px`;

        document.getElementById(containerID)!.appendChild(anchor);

        setContextMenuAnchor(anchor);
    };

    const hideContextMenu = () => {
        if (!contextMenuAnchor) return;

        contextMenuAnchor.remove();
        setContextMenuAnchor(undefined);
    };

    return (
        <div>
            <div id={containerID} className="relative">
                <div className="bg-white dark:bg-gray-900 border-2 border-gray-100 dark:border-gray-700 rounded-lg overflow-clip">
                    <SimpleCodeEditor
                        id={editorID}
                        textareaId={textAreaID}
                        className="code-editor m-4 dark:caret-white"
                        textareaClassName="outline-0"
                        preClassName="language-wipple"
                        style={{
                            fontFamily: "'JetBrains Mono', monospace",
                            fontStyle: props.code ? "normal" : "italic",
                            fontVariantLigatures: "none",
                            wordWrap: "break-word",
                            tabSize: 2,
                        }}
                        value={props.code}
                        onValueChange={(code) => {
                            setHover(undefined);
                            props.onChange(code);
                        }}
                        highlight={(code) =>
                            prism.highlight(code, prism.languages.wipple, "wipple")
                        }
                        tabSize={1}
                        insertSpaces={false}
                        placeholder="Write your code here!"
                        autoFocus={props.autoFocus}
                    />

                    <animated.div style={animatedOutputStyle}>
                        <div ref={outputRef}>
                            {(() => {
                                if (output == null) {
                                    return null;
                                }

                                return (
                                    <div>
                                        {output.diagnostics.length ? (
                                            <div
                                                className={`p-4 flex flex-col gap-4 text-black dark:text-white ${
                                                    output.diagnostics.find(
                                                        ({ level }) => level === "error"
                                                    )
                                                        ? "bg-red-50 dark:bg-red-900 dark:bg-opacity-40"
                                                        : "bg-yellow-50 dark:bg-yellow-900 dark:bg-opacity-40"
                                                }`}
                                            >
                                                {output.diagnostics.map((diagnostic, index) => (
                                                    <div key={index} className="flex gap-2">
                                                        <div
                                                            className={`rounded-sm border-r-4 ${
                                                                diagnostic.level === "error"
                                                                    ? "border-r-red-500"
                                                                    : "border-r-yellow-500"
                                                            }`}
                                                        />

                                                        <div
                                                            key={index}
                                                            className="flex flex-col gap-2.5"
                                                        >
                                                            <div
                                                                className={`font-bold ${
                                                                    diagnostic.level === "error"
                                                                        ? "text-red-600 dark:text-red-500"
                                                                        : "text-yellow-600 dark:text-yellow-500"
                                                                }`}
                                                            >
                                                                <ReactMarkdown
                                                                    remarkPlugins={[
                                                                        remarkMath,
                                                                        remarkGfm,
                                                                        remarkSmartypants,
                                                                    ]}
                                                                    rehypePlugins={[
                                                                        rehypeRaw,
                                                                        rehypeKatex,
                                                                    ]}
                                                                    linkTarget="_blank"
                                                                >
                                                                    {`${diagnostic.level}: ${diagnostic.message}`}
                                                                </ReactMarkdown>
                                                            </div>

                                                            {diagnostic.notes.map(
                                                                (note, noteIndex) => {
                                                                    const lookup = lineColumn(
                                                                        note.code + "\n\n"
                                                                    );

                                                                    let { line, col } =
                                                                        lookup.fromIndex(
                                                                            note.span.start
                                                                        )!;

                                                                    const start = lookup.toIndex(
                                                                        line,
                                                                        1
                                                                    );

                                                                    const end = lookup.toIndex(
                                                                        line + 1,
                                                                        1
                                                                    );

                                                                    return (
                                                                        <div
                                                                            className="flex flex-col"
                                                                            key={noteIndex}
                                                                        >
                                                                            <div>
                                                                                <p className="semibold text-xs opacity-50">
                                                                                    {note.span
                                                                                        .file ===
                                                                                    "playground"
                                                                                        ? ""
                                                                                        : `${note.span.file}, `}
                                                                                    line {line}
                                                                                </p>

                                                                                <pre>
                                                                                    <span>
                                                                                        {note.code.slice(
                                                                                            start,
                                                                                            note
                                                                                                .span
                                                                                                .start
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
                                                                                            note
                                                                                                .span
                                                                                                .start,
                                                                                            note
                                                                                                .span
                                                                                                .end
                                                                                        )}
                                                                                    </span>

                                                                                    <span>
                                                                                        {note.code.slice(
                                                                                            note
                                                                                                .span
                                                                                                .end,
                                                                                            end
                                                                                        )}
                                                                                    </span>
                                                                                </pre>
                                                                            </div>

                                                                            {note.messages.map(
                                                                                (
                                                                                    message,
                                                                                    messageIndex
                                                                                ) => (
                                                                                    <div
                                                                                        key={
                                                                                            messageIndex
                                                                                        }
                                                                                        className={`flex ${
                                                                                            noteIndex ===
                                                                                                0 &&
                                                                                            messageIndex ===
                                                                                                0
                                                                                                ? diagnostic.level ===
                                                                                                  "error"
                                                                                                    ? "text-red-600 dark:text-red-500"
                                                                                                    : "text-yellow-600 dark:text-yellow-500"
                                                                                                : "opacity-75"
                                                                                        }`}
                                                                                    >
                                                                                        <pre>
                                                                                            {new Array(
                                                                                                col -
                                                                                                    1
                                                                                            )
                                                                                                .fill(
                                                                                                    " "
                                                                                                )
                                                                                                .join(
                                                                                                    ""
                                                                                                )}
                                                                                        </pre>

                                                                                        <ReactMarkdown
                                                                                            remarkPlugins={[
                                                                                                remarkMath,
                                                                                                remarkGfm,
                                                                                                remarkSmartypants,
                                                                                            ]}
                                                                                            rehypePlugins={[
                                                                                                rehypeRaw,
                                                                                                rehypeKatex,
                                                                                            ]}
                                                                                            linkTarget="_blank"
                                                                                        >
                                                                                            {
                                                                                                message
                                                                                            }
                                                                                        </ReactMarkdown>
                                                                                    </div>
                                                                                )
                                                                            )}
                                                                        </div>
                                                                    );
                                                                }
                                                            )}
                                                        </div>
                                                    </div>
                                                ))}
                                            </div>
                                        ) : null}

                                        {output.items.length ? (
                                            <div className="p-4 bg-gray-50 dark:bg-gray-800 text-black dark:text-white">
                                                {output.items.map((item, index) => {
                                                    switch (item.type) {
                                                        case "output":
                                                            return (
                                                                <div
                                                                    key={index}
                                                                    className="prose prose-sky dark:prose-invert max-w-none"
                                                                >
                                                                    <ReactMarkdown
                                                                        remarkPlugins={[
                                                                            remarkMath,
                                                                            remarkGfm,
                                                                            remarkSmartypants,
                                                                        ]}
                                                                        rehypePlugins={[
                                                                            rehypeRaw,
                                                                            rehypeKatex,
                                                                        ]}
                                                                        linkTarget="_blank"
                                                                    >
                                                                        {item.text}
                                                                    </ReactMarkdown>
                                                                </div>
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

                                                {isRunning ? (
                                                    <div className="bouncing-loader">
                                                        <div />
                                                        <div />
                                                        <div />
                                                    </div>
                                                ) : (Array.isArray(output) &&
                                                      output.find(
                                                          (item) => item.type !== "output"
                                                      )) ||
                                                  uiElements.current.length ? (
                                                    <div className="mt-4">
                                                        <Button
                                                            variant="contained"
                                                            size="small"
                                                            endIcon={<Refresh />}
                                                            onClick={() =>
                                                                run(props.code, props.lint)
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
                        </div>
                    </animated.div>
                </div>

                {completions && (
                    <>
                        <animated.div className="absolute" style={animatedContextMenuTriggerStyle}>
                            <IconButton
                                color="primary"
                                onMouseDown={(e) => {
                                    e.preventDefault();
                                    showContextMenu();
                                }}
                            >
                                <Add />
                            </IconButton>
                        </animated.div>

                        <Menu
                            open={contextMenuAnchor != null}
                            anchorEl={contextMenuAnchor}
                            onClose={hideContextMenu}
                            style={{ maxHeight: 500 }}
                        >
                            <MenuList disablePadding>
                                {(() => {
                                    const renderCompletionItem = (
                                        { name, kind, help }: Completion,
                                        index: number
                                    ) =>
                                        help ? (
                                            <MenuItem
                                                key={index}
                                                onClick={() => {
                                                    const code = props.code;

                                                    const before = code.slice(
                                                        0,
                                                        textEditor!.selectionEnd
                                                    );
                                                    const padBefore = (
                                                        before[before.length - 1] ?? " "
                                                    ).match(/\s/)
                                                        ? ""
                                                        : " ";

                                                    const after = code.slice(
                                                        textEditor!.selectionEnd
                                                    );
                                                    const padAfter = (after[0] ?? " ").match(/\s/)
                                                        ? ""
                                                        : " ";

                                                    props.onChange(
                                                        before + padBefore + name + padAfter + after
                                                    );

                                                    hideContextMenu();
                                                }}
                                                style={{ maxWidth: 400, whiteSpace: "normal" }}
                                            >
                                                <ListItemText>
                                                    <pre className="language-wipple">
                                                        <span className={`token ${kind}`}>
                                                            {name}
                                                        </span>
                                                    </pre>

                                                    <ReactMarkdown
                                                        remarkPlugins={[
                                                            remarkMath,
                                                            remarkGfm,
                                                            remarkSmartypants,
                                                        ]}
                                                        rehypePlugins={[rehypeRaw, rehypeKatex]}
                                                        linkTarget="_blank"
                                                    >
                                                        {help}
                                                    </ReactMarkdown>
                                                </ListItemText>
                                            </MenuItem>
                                        ) : null;

                                    const variablesSection =
                                        completions.variables.map(renderCompletionItem);

                                    const groupedConstantsSection =
                                        completions.groupedConstants.map(
                                            ([group, completions], index) => (
                                                <div key={index}>
                                                    <Divider />
                                                    <ListSubheader>{group}</ListSubheader>
                                                    <Divider />
                                                    {...completions.map(renderCompletionItem)}
                                                </div>
                                            )
                                        );

                                    const ungroupedConstantsSection =
                                        completions.ungroupedConstants.map(renderCompletionItem);

                                    return (
                                        <>
                                            {variablesSection}
                                            {groupedConstantsSection}
                                            {ungroupedConstantsSection.length && <Divider />}
                                            {ungroupedConstantsSection}
                                        </>
                                    );
                                })()}
                            </MenuList>
                        </Menu>
                    </>
                )}
            </div>

            {hover && (
                <div
                    className="absolute mt-2 p-2 overflow-clip bg-white dark:bg-gray-900 border-2 border-gray-100 dark:border-gray-700 rounded-lg text-black dark:text-white"
                    style={{
                        left: hover.x,
                        top: hover.y,
                        maxWidth: 400,
                        zIndex: 9999,
                    }}
                >
                    {hover.output.code ? (
                        <div className="pointer-events-none">
                            <SimpleCodeEditor
                                className="code-editor dark:caret-white"
                                textareaClassName="outline-0"
                                preClassName="language-wipple"
                                style={{
                                    fontFamily: "'JetBrains Mono', monospace",
                                    fontStyle: props.code ? "normal" : "italic",
                                    fontVariantLigatures: "none",
                                    wordWrap: "break-word",
                                }}
                                value={hover.output.code}
                                highlight={(code) =>
                                    prism.highlight(code, prism.languages.wipple, "wipple")
                                }
                                onValueChange={() => {}}
                                contentEditable={false}
                            />

                            {hover.output.help ? <p>{hover.output.help}</p> : null}
                        </div>
                    ) : null}
                </div>
            )}
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

    return (
        <div className={props.index === 0 ? "mb-4" : "my-4"}>
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
                            <IconButton
                                disabled={!isEnabled}
                                onClick={async () => {
                                    const valid = await props.onSubmit(text);
                                    setValid(valid);
                                    if (valid) {
                                        setEnabled(false);
                                    }
                                }}
                            >
                                <KeyboardReturn />
                            </IconButton>
                        </InputAdornment>
                    ),
                }}
            />
        </div>
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
