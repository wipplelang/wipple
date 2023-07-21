import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import SimpleCodeEditor from "./react-simple-code-editor";
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
    Tooltip,
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
    AnalysisConsoleDiagnosticFix,
} from "../../runner";
import KeyboardReturn from "@mui/icons-material/KeyboardReturn";
import Refresh from "@mui/icons-material/Refresh";
import AddRounded from "@mui/icons-material/AddRounded";
import SubjectRounded from "@mui/icons-material/SubjectRounded";
import PlayArrowRounded from "@mui/icons-material/PlayArrowRounded";
import PauseRounded from "@mui/icons-material/PauseRounded";
import FullScreenRounded from "@mui/icons-material/FullscreenRounded";
import FullScreenExitRounded from "@mui/icons-material/FullscreenExitRounded";
import getCaretCoordinates from "textarea-caret";
import ErrorIcon from "@mui/icons-material/Error";
import lineColumn from "line-column";
import { useRefState } from "../helpers";
import { Settings } from "../App";
import * as Sentry from "@sentry/react";

export interface CodeEditorProps {
    id: string;
    code: string;
    autoRun: boolean;
    onChangeAutoRun: (autoRun: boolean) => void;
    lint: boolean;
    collapse: boolean;
    onChangeCollapse: (collapse: boolean) => void;
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
    output: HoverOutput | null;
    diagnostic:
        | [AnalysisOutputDiagnostic, boolean, AnalysisOutputDiagnostic["notes"][number]]
        | undefined;
}

interface UiElement {
    onMessage: Record<string, (message: string, value: any) => Promise<any>>;
}

const closingBrackets: Record<string, string> = {
    "(": ")",
    "{": "}",
    "[": "]",
};

export const CodeEditor = (props: CodeEditorProps) => {
    const containerID = `code-editor-container-${props.id}`;
    const editorID = `code-editor-editor-${props.id}`;
    const textAreaID = `code-editor-text-${props.id}`;

    const codeEditorRef = useRef<SimpleCodeEditor>(null);

    const [syntaxHighlighting, setSyntaxHighlighting] = useState<
        AnalysisOutputSyntaxHighlightingItem[]
    >([]);

    const [isRunning, setRunning] = useState(false);
    const [output, setOutput] = useRefState<
        { code: string; items: OutputItem[]; diagnostics: AnalysisOutputDiagnostic[] } | undefined
    >(undefined);
    const appendToOutput = (code: string, item: OutputItem) =>
        setOutput((output) =>
            output
                ? { code, items: [...output.items, item], diagnostics: output.diagnostics }
                : { code, items: [item], diagnostics: [] }
        );

    const [fatalError, setFatalError] = useState(false);

    const [completions, setCompletions] = useState<AnalysisOutputCompletions>();

    const [outputRef, { height: outputHeight }] = useMeasure();
    const animatedOutputStyle = useSpring(
        fatalError || output.current != null
            ? { opacity: 1, height: outputHeight }
            : { opacity: 0, height: 0 }
    );

    const prefersReducedMotion = useMediaQuery("(prefers-reduced-motion)");
    useEffect(() => {
        SpringGlobals.assign({
            skipAnimation: prefersReducedMotion,
        });
    }, [prefersReducedMotion]);

    const [uiElements, setUiElements] = useRefState<UiElement[]>([]);
    const [currentUiElementId, setCurrentUiElementId] = useRefState("");

    const [containsTemplates, setContainsTemplates] = useRefState(false);
    const [showTemplatesWarning, setShowTemplatesWarning] = useState(false);

    const canCollapse = props.code.length > 0 && (output.current?.items.length ?? 0) > 0;

    const [codeEditorContainerRef, { height: codeEditorContainerHeight }] = useMeasure();
    const animatedCodeEditorStyle = useSpring(
        canCollapse && props.collapse
            ? { opacity: 0, height: 0 }
            : { opacity: 1, height: codeEditorContainerHeight }
    );

    const runner = useRunner({ id: props.id, code: props.code });

    const run = useMemo(
        () =>
            debounce(async (code: string, lint: boolean) => {
                try {
                    setRunning(true);
                    setSyntaxHighlighting([]); // FIXME: Prevent flashing
                    setFatalError(false);

                    const analysis = await runner.analyze(
                        code,
                        lint,
                        async (url, name, input, api) => {
                            console.log("Received plugin request:", { url, name, input, api });

                            const plugin = await import(/* @vite-ignore */ url);

                            if (!(name in plugin)) {
                                throw new Error(`no such plugin '${name}' in file`);
                            }

                            return plugin[name](input, api);
                        }
                    );

                    setSyntaxHighlighting(analysis.syntaxHighlighting);

                    setOutput({
                        code: code,
                        items: [],
                        diagnostics: containsTemplates.current ? [] : analysis.diagnostics,
                    });

                    setCompletions(analysis.completions);
                    setUiElements([]);
                    setShowTemplatesWarning(containsTemplates.current);

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
                                Sentry.captureException(error, (ctx) => {
                                    ctx.setContext("code-editor", { ...props });
                                    return ctx;
                                });

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
        [props.id]
    );

    useEffect(() => {
        const containsTemplates =
            document.querySelectorAll<HTMLSpanElement>(
                `#${editorID} .language-wipple span.token.template-content`
            ).length > 0;

        setContainsTemplates(containsTemplates);

        if (props.autoRun) {
            run(props.code, props.lint);
        }
    }, [run, props.code, props.lint, props.autoRun]);

    const [hover, setHover] = useState<Hover>();

    const container = document.getElementById(containerID) as HTMLDivElement | null;
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
        if (!container || !textEditor) return;

        let hoverTimer: NodeJS.Timeout | undefined = undefined;
        let hoverElement: HTMLElement | undefined = undefined;

        const hoverClasses = ["bg-black", "dark:bg-white", "bg-opacity-10", "dark:bg-opacity-10"];

        const handleMouseMove = (e: MouseEvent) => {
            const mouseX = e.x;
            const mouseY = e.y;

            const rectContainsMouse = (rect: DOMRect) =>
                mouseX >= rect.left &&
                mouseX < rect.right &&
                mouseY >= rect.top &&
                mouseY < rect.bottom;

            if (hoverElement) {
                if (rectContainsMouse(hoverElement.getBoundingClientRect())) {
                    return;
                }
            }

            const hover = document.querySelector(`${containerID} #hover`);

            if (hover && rectContainsMouse(hover.getBoundingClientRect())) {
                return;
            }

            let foundHoverElement = false;
            for (const el of document.querySelectorAll<HTMLSpanElement>(`#${editorID} .token`)) {
                if (rectContainsMouse(el.getBoundingClientRect())) {
                    setHover(undefined);
                    hoverElement?.classList.remove(...hoverClasses);
                    hoverElement = el;
                    foundHoverElement = true;
                    break;
                }
            }

            if (!foundHoverElement) {
                setHover(undefined);
                hoverElement?.classList.remove(...hoverClasses);
                hoverElement = undefined;
                return;
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

                const start = parseInt(hoverElement.dataset.wippleStartIndex);
                const end = parseInt(hoverElement.dataset.wippleEndIndex);

                let hoverDiagnostic: Hover["diagnostic"];
                outer: for (const diagnostic of output.current?.diagnostics ?? []) {
                    for (let noteIndex = 0; noteIndex < diagnostic.notes.length; noteIndex++) {
                        const note = diagnostic.notes[noteIndex];

                        if (note.span.start >= start && note.span.end <= end) {
                            hoverDiagnostic = [diagnostic, noteIndex === 0, note];
                            break outer;
                        }
                    }
                }

                const hoverOutput = await runner.hover(start, end);

                if (hoverOutput || hoverDiagnostic) {
                    hoverElement.classList.add(...hoverClasses);

                    setHover({
                        x: hoverElement.getBoundingClientRect().x,
                        y: hoverElement.getBoundingClientRect().bottom + window.scrollY,
                        output: hoverOutput,
                        diagnostic: hoverDiagnostic,
                    });
                }
            }, 250);
        };

        const handleMouseLeave = () => {
            setHover(undefined);
            hoverElement?.classList.remove(...hoverClasses);
            hoverElement = undefined;
        };

        container.addEventListener("mouseleave", handleMouseLeave);
        textEditor.addEventListener("mousemove", handleMouseMove);

        return () => {
            container.removeEventListener("mouseleave", handleMouseLeave);
            textEditor.removeEventListener("mousemove", handleMouseMove);
        };
    }, [container, textEditor]);

    useEffect(() => {
        const nodes = [
            ...document.querySelectorAll<HTMLSpanElement>(
                `#${editorID} .language-wipple span.token`
            ),
        ];

        const colors: string[][] = [
            ["bg-blue-500", "dark:bg-blue-400"],
            ["bg-red-500", "dark:bg-red-400"],
            ["bg-green-500", "dark:bg-green-400"],
            ["bg-yellow-500", "dark:bg-yellow-400"],
        ];

        const additionalClasses = ["bg-opacity-20", "dark:bg-opacity-20"];

        if (!(props.settings.beginner ?? true)) {
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
            if (closingBrackets[node.innerText]) {
                color = colors[stack.length % colors.length];
                stack.push(color);
            } else if (Object.values(closingBrackets).includes(node.innerText)) {
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

            if (!currentNode.classList.contains("diagnostic")) {
                currentNode.classList.add(item.kind);
                currentNode.dataset.wippleStartIndex = item.start.toString();
                currentNode.dataset.wippleEndIndex = item.end.toString();
            }
        }
    }, [syntaxHighlighting]);

    useEffect(() => {
        if (!output.current?.diagnostics) return;

        const forEachNode = (f: (start: number, end: number, node: HTMLSpanElement) => boolean) => {
            const nodes = [
                ...document.querySelectorAll<HTMLSpanElement>(
                    `#${editorID} .language-wipple span.token`
                ),
            ];

            let currentNode = nodes.shift();
            if (!currentNode) return;

            let start = 0;
            while (!f(start, start + currentNode.innerText.length, currentNode)) {
                start += currentNode.innerText.length;
                currentNode = nodes.shift();
                if (!currentNode) return;
            }
        };

        forEachNode((_start, _end, node) => {
            node.classList.remove(
                "diagnostic",
                "diagnostic-error",
                "diagnostic-warning",
                "diagnostic-note"
            );

            return false;
        });

        for (const diagnostic of output.current.diagnostics) {
            const notes = [...diagnostic.notes];
            const primaryNote = notes.shift();
            if (!primaryNote) {
                continue;
            }

            forEachNode((start, end, node) => {
                if (end > primaryNote.span.end) {
                    return true;
                }

                if (start >= primaryNote.span.start) {
                    node.classList.add("diagnostic", `diagnostic-${diagnostic.level}`);
                    node.dataset.wippleStartIndex = primaryNote.span.start.toString();
                    node.dataset.wippleEndIndex = primaryNote.span.end.toString();
                }

                return false;
            });

            forEachNode((start, end, node) => {
                const note = notes[0];
                if (!note || end > note.span.end) {
                    notes.shift();
                    return true;
                }

                if (start >= note.span.start && !node.classList.contains("diagnostic")) {
                    node.classList.add("diagnostic", "diagnostic-note");
                    node.dataset.wippleStartIndex = note.span.start.toString();
                    node.dataset.wippleEndIndex = note.span.end.toString();
                }

                return false;
            });
        }
    }, [output.current?.diagnostics]);

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
    const [contextMenuSearch, setContextMenuSearch] = useState("");

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
        setContextMenuSearch("");
    };

    const buttonIconStyles = {
        fontSize: "14pt",
        width: 26,
        marginTop: "-0.125rem",
    };

    const insertCompletion = (completion: Completion) => {
        const code = props.code;

        const before = code.slice(0, textEditor!.selectionEnd);
        const padBefore = (before[before.length - 1] ?? " ").match(/\s/) ? "" : " ";

        const after = code.slice(textEditor!.selectionEnd);
        const padAfter = (after[0] ?? " ").match(/\s/) ? "" : " ";

        props.onChange(before + padBefore + completion.template + padAfter + after);
    };

    useEffect(() => {
        const textArea = document.getElementById(textAreaID)! as HTMLTextAreaElement;

        let isSelectingGroup = false;
        const handler = (e: MouseEvent | KeyboardEvent) => {
            requestAnimationFrame(() => {
                let isAttemptingSelection = false;
                if (e instanceof MouseEvent) {
                    isAttemptingSelection = textArea.selectionStart === textArea.selectionEnd;
                } else if (e instanceof KeyboardEvent) {
                    isAttemptingSelection = e.key === "ArrowLeft" || e.key === "ArrowRight";
                }

                if (!isAttemptingSelection) {
                    return;
                }

                if (isSelectingGroup) {
                    textArea.setSelectionRange(textArea.selectionEnd, textArea.selectionEnd);
                    isSelectingGroup = false;
                    return;
                }

                const nodes = [
                    ...document.querySelectorAll<HTMLSpanElement>(
                        `#${editorID} .language-wipple span.token`
                    ),
                ];

                interface NodeGroup {
                    start: number;
                    end: number;
                }

                const nodeGroups: NodeGroup[] = [];
                let start = 0;

                while (nodes.length !== 0) {
                    const node = nodes.shift()!;

                    if (node.classList.contains("template-before")) {
                        nodeGroups.push({ start, end: start });
                    } else if (node.classList.contains("template-after")) {
                        nodeGroups[nodeGroups.length - 1].end = start + node.innerText.length;
                    }

                    start += node.innerText.length;
                }

                const nodeGroup = nodeGroups.find(
                    (group) =>
                        textArea.selectionStart >= group.start && textArea.selectionEnd <= group.end
                );

                if (nodeGroup) {
                    textArea.setSelectionRange(nodeGroup.start, nodeGroup.end, "forward");
                }

                isSelectingGroup = nodeGroup != null;
            });
        };

        textArea.addEventListener("mousedown", handler);
        textArea.addEventListener("keydown", handler);

        return () => {
            textArea.removeEventListener("mousedown", handler);
            textArea.removeEventListener("keydown", handler);
        };
    }, []);

    const applyFix = (fix: AnalysisConsoleDiagnosticFix) => {
        const fixed = props.code.slice(0, fix.start) + fix.replacement + props.code.slice(fix.end);
        props.onChange(fixed);
    };

    return (
        <div id={containerID}>
            <div className="relative -mt-3.5">
                <div className="flex flex-row justify-end w-full pr-4 -mb-3.5">
                    <div className="code-editor-outlined rounded-md shadow-lg shadow-gray-100 dark:shadow-gray-900 h-7 text-gray-500 text-opacity-50">
                        <Tooltip title="Insert">
                            <button
                                className="code-editor-button -ml-0.5"
                                onMouseDown={(e) => {
                                    e.preventDefault();
                                    showContextMenu();
                                }}
                            >
                                <AddRounded sx={buttonIconStyles} />
                            </button>
                        </Tooltip>

                        <Tooltip title="Format">
                            <button
                                className="code-editor-button -mx-0.5"
                                disabled={props.code.length === 0}
                                onMouseDown={async (e) => {
                                    const formatted = await runner.format(props.code);

                                    if (formatted != null) {
                                        const codeEditor = document.getElementById(
                                            textAreaID
                                        ) as HTMLTextAreaElement | null;
                                        if (!codeEditor) return;

                                        codeEditorRef.current!.session.history.stack.push({
                                            timestamp: new Date().valueOf(),
                                            value: props.code,
                                            selectionStart: codeEditor.selectionStart,
                                            selectionEnd: codeEditor.selectionEnd,
                                        });

                                        props.onChange(formatted.trimEnd());
                                    }
                                }}
                            >
                                <SubjectRounded sx={buttonIconStyles} />
                            </button>
                        </Tooltip>

                        <Tooltip
                            title={
                                containsTemplates.current
                                    ? "Running Paused"
                                    : props.autoRun
                                    ? "Pause Running"
                                    : "Run"
                            }
                        >
                            <button
                                className="code-editor-button -mr-0.5"
                                onMouseDown={(e) => {
                                    if (containsTemplates.current) {
                                        return;
                                    }

                                    props.onChangeAutoRun(!props.autoRun);
                                }}
                            >
                                {props.autoRun && !containsTemplates.current ? (
                                    <PauseRounded sx={buttonIconStyles} />
                                ) : (
                                    <PlayArrowRounded sx={buttonIconStyles} />
                                )}
                            </button>
                        </Tooltip>

                        <Tooltip title={canCollapse && props.collapse ? "Expand" : "Collapse"}>
                            <button
                                className="code-editor-button -mr-0.5"
                                disabled={!canCollapse}
                                onMouseDown={(e) => {
                                    if (!canCollapse) {
                                        return;
                                    }

                                    props.onChangeCollapse(!props.collapse);
                                }}
                            >
                                {canCollapse && props.collapse ? (
                                    <FullScreenRounded sx={buttonIconStyles} />
                                ) : (
                                    <FullScreenExitRounded sx={buttonIconStyles} />
                                )}
                            </button>
                        </Tooltip>
                    </div>
                </div>

                <div className="code-editor-outlined rounded-lg">
                    <animated.div style={animatedCodeEditorStyle}>
                        <div ref={codeEditorContainerRef} className="p-4">
                            <SimpleCodeEditor
                                ref={codeEditorRef}
                                id={editorID}
                                textareaId={textAreaID}
                                className="code-editor dark:caret-white"
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
                                placeholder="Write your code here!"
                                autoFocus={props.autoFocus}
                            />
                        </div>
                    </animated.div>

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
                                                Wipple encountered an internal error while running
                                                your code. Please reload the page and try again.
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
                                                Your code contains placeholders that need to be
                                                filled before your program can be run.
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
                                            props.settings.beginner ?? true ? (
                                                output.current.diagnostics.find(
                                                    ({ level }) => level === "error"
                                                ) ? (
                                                    <div className="flex flex-col gap-4 p-6 text-red-500 bg-red-50 dark:bg-red-900 dark:bg-opacity-20">
                                                        <div className="flex items-center gap-2">
                                                            <ErrorIcon fontSize="large" />
                                                            <h1 className="text-xl">Error</h1>
                                                        </div>

                                                        <p className="text-gray-500 dark:text-gray-400">
                                                            Wipple couldn’t run your code because it
                                                            has errors.
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
                                                    {output.current.diagnostics.map(
                                                        (diagnostic, index) => (
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
                                                                            diagnostic.level ===
                                                                            "error"
                                                                                ? "text-red-600 dark:text-red-500"
                                                                                : "text-yellow-600 dark:text-yellow-500"
                                                                        }`}
                                                                    >
                                                                        <Markdown>
                                                                            {`${diagnostic.level}: ${diagnostic.message}`}
                                                                        </Markdown>
                                                                    </div>

                                                                    {diagnostic.notes.map(
                                                                        (note, noteIndex) => {
                                                                            const lookup =
                                                                                lineColumn(
                                                                                    note.code +
                                                                                        "\n\n"
                                                                                );

                                                                            let { line, col } =
                                                                                lookup.fromIndex(
                                                                                    note.span.start
                                                                                )!;

                                                                            const start =
                                                                                lookup.toIndex(
                                                                                    line,
                                                                                    1
                                                                                );

                                                                            const end =
                                                                                lookup.toIndex(
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
                                                                                            {note
                                                                                                .span
                                                                                                .file ===
                                                                                            "playground"
                                                                                                ? ""
                                                                                                : `${note.span.file}, `}
                                                                                            line{" "}
                                                                                            {line}
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

                                                                                                <Markdown>
                                                                                                    {
                                                                                                        message
                                                                                                    }
                                                                                                </Markdown>
                                                                                            </div>
                                                                                        )
                                                                                    )}
                                                                                </div>
                                                                            );
                                                                        }
                                                                    )}
                                                                </div>
                                                            </div>
                                                        )
                                                    )}
                                                </div>
                                            )
                                        ) : null}

                                        {output.current.items.length ? (
                                            <div className="p-4 bg-gray-50 dark:bg-gray-800 text-black dark:text-white">
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

                                                {isRunning ? (
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
                    <Menu
                        open={contextMenuAnchor != null}
                        anchorEl={contextMenuAnchor}
                        onClose={hideContextMenu}
                        style={{ maxHeight: 500 }}
                    >
                        <TextField
                            inputMode="search"
                            value={contextMenuSearch}
                            onChange={(e) => setContextMenuSearch(e.target.value)}
                            placeholder="Search"
                            fullWidth
                        />

                        <MenuList disablePadding>
                            {(() => {
                                const includeCompletion = (completion: Completion) =>
                                    !contextMenuSearch ||
                                    contextMenuSearch.includes(completion.name) ||
                                    completion.name.includes(contextMenuSearch);

                                const renderCompletionItem = (
                                    completion: Completion,
                                    index: number
                                ) =>
                                    completion.help ? (
                                        <MenuItem
                                            key={index}
                                            onClick={() => {
                                                insertCompletion(completion);
                                                hideContextMenu();
                                            }}
                                            style={{ maxWidth: 400, whiteSpace: "normal" }}
                                        >
                                            <ListItemText>
                                                <pre className="language-wipple">
                                                    <span className={`token ${completion.kind}`}>
                                                        {completion.name}
                                                    </span>
                                                </pre>

                                                <Markdown>{completion.help}</Markdown>
                                            </ListItemText>
                                        </MenuItem>
                                    ) : null;

                                const languageSection = completions.language
                                    .filter(includeCompletion)
                                    .map(renderCompletionItem);

                                const variablesSection = completions.variables
                                    .filter(includeCompletion)
                                    .map(renderCompletionItem);

                                const groupedConstantsSection = completions.groupedConstants.map(
                                    ([group, completions], index) => {
                                        const filtered = completions.filter(includeCompletion);

                                        return filtered.length ? (
                                            <div key={index}>
                                                <Divider />
                                                <ListSubheader>{group}</ListSubheader>
                                                <Divider />
                                                {...filtered.map(renderCompletionItem)}
                                            </div>
                                        ) : null;
                                    }
                                );

                                const ungroupedConstantsSection = completions.ungroupedConstants
                                    .filter(includeCompletion)
                                    .map(renderCompletionItem);

                                return [
                                    languageSection,
                                    languageSection.length ? (
                                        <Divider key="languageDivider" />
                                    ) : null,
                                    variablesSection,
                                    variablesSection.length ? (
                                        <Divider key="variablesDivider" />
                                    ) : null,
                                    groupedConstantsSection,
                                    ungroupedConstantsSection.length ? (
                                        <Divider key="ungroupedConstantsDivider" />
                                    ) : null,
                                    ungroupedConstantsSection,
                                ];
                            })()}
                        </MenuList>
                    </Menu>
                )}
            </div>

            {hover && (
                <div
                    id="hover"
                    className="absolute pt-2"
                    style={{
                        left: hover.x,
                        top: hover.y,
                        maxWidth: 400,
                        zIndex: 9999,
                    }}
                >
                    <div className="p-2 overflow-clip bg-white dark:bg-gray-900 border-2 border-gray-100 dark:border-gray-700 rounded-lg text-black dark:text-white">
                        {hover.diagnostic ? (
                            <div className="flex flex-col">
                                <div
                                    className={`font-bold ${
                                        hover.diagnostic[0].level === "error"
                                            ? "text-red-600 dark:text-red-500"
                                            : "text-yellow-600 dark:text-yellow-500"
                                    }`}
                                >
                                    <Markdown>
                                        {`${hover.diagnostic[0].level}: ${hover.diagnostic[0].message}`}
                                    </Markdown>
                                </div>

                                <div className="flex flex-col">
                                    {hover.diagnostic[2].messages.map((message, messageIndex) => (
                                        <div
                                            key={messageIndex}
                                            className={
                                                messageIndex === 0 && hover.diagnostic![1]
                                                    ? hover.diagnostic![0].level === "error"
                                                        ? "text-red-600 dark:text-red-500"
                                                        : "text-yellow-600 dark:text-yellow-500"
                                                    : "opacity-75"
                                            }
                                        >
                                            <Markdown>{message}</Markdown>
                                        </div>
                                    ))}
                                </div>

                                {hover.diagnostic[0].fix && (
                                    <div className="flex">
                                        <button
                                            className="mt-1.5 px-1.5 py-0.5 rounded-md bg-blue-500 text-white"
                                            onClick={() => {
                                                applyFix(hover.diagnostic![0].fix!);
                                                setHover(undefined);
                                            }}
                                        >
                                            <Markdown>
                                                {hover.diagnostic[0].fix.description}
                                            </Markdown>
                                        </button>
                                    </div>
                                )}

                                {hover.output && (
                                    <div className="h-0.5 my-2 bg-gray-100 dark:bg-gray-700"></div>
                                )}
                            </div>
                        ) : null}

                        {hover.output?.code ? (
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

                                {hover.output.help ? (
                                    <Markdown>{hover.output.help}</Markdown>
                                ) : null}
                            </div>
                        ) : null}

                        {hover.output?.url ? (
                            <div className="mt-1.5">
                                <a
                                    href={hover.output.url}
                                    target="_blank"
                                    className="px-1.5 py-0.5 rounded-md bg-blue-500 text-white"
                                >
                                    Documentation
                                </a>
                            </div>
                        ) : null}
                    </div>
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

const Markdown = (props: { children: string; className?: string }) => (
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
