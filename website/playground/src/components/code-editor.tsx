import { useCallback, useEffect, useMemo, useState } from "react";
import SimpleCodeEditor from "react-simple-code-editor";
import * as prism from "prismjs";
import {
    Button,
    debounce,
    FormControl,
    IconButton,
    InputAdornment,
    InputLabel,
    ListItemText,
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
    AnalysisOutputDiagnostics,
    AnalysisOutputSyntaxHighlightingItem,
    HoverOutput,
    AnalysisOutputCompletionItem,
    useRunner,
} from "../../runner";
import KeyboardReturn from "@mui/icons-material/KeyboardReturn";
import Refresh from "@mui/icons-material/Refresh";
import Add from "@mui/icons-material/Add";
import getCaretCoordinates from "textarea-caret";

export interface CodeEditorProps {
    id: string;
    code: string;
    lint: boolean;
    autoFocus: boolean;
    onChange: (code: string) => void;
}

type OutputItem =
    | { type: "output"; text: string }
    | { type: "prompt"; prompt: string; onSubmit: (text: string) => Promise<boolean> }
    | { type: "choice"; prompt: string; choices: string[]; onSubmit: (index: number) => void };

interface Hover {
    x: number;
    y: number;
    output: HoverOutput;
}

export const CodeEditor = (props: CodeEditorProps) => {
    const containerID = `code-editor-container-${props.id}`;
    const editorID = `code-editor-editor-${props.id}`;
    const textAreaID = `code-editor-text-${props.id}`;

    const [syntaxHighlighting, setSyntaxHighlighting] = useState<
        AnalysisOutputSyntaxHighlightingItem[]
    >([]);

    const [isRunning, setRunning] = useState(false);
    const [output, setOutput] = useState<AnalysisOutputDiagnostics | OutputItem[] | undefined>();
    const appendToOutput = (item: OutputItem) =>
        setOutput((output) => (Array.isArray(output) ? [...output, item] : [item]));

    const [completions, setCompletions] = useState<AnalysisOutputCompletionItem[]>([]);

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

    const runner = useRunner();

    const run = useMemo(
        () =>
            debounce(async (code: string, lint: boolean) => {
                try {
                    setSyntaxHighlighting([]); // FIXME: Prevent flashing
                    const analysis = await runner.analyze(code, lint);
                    setSyntaxHighlighting(analysis.syntaxHighlighting);
                    setOutput(analysis.diagnostics);
                    setCompletions(analysis.completions);

                    if (analysis.diagnostics.type !== "error") {
                        setOutput([]);
                        setRunning(true);

                        const success = await runner.run((request) => {
                            switch (request.type) {
                                case "display":
                                    appendToOutput({
                                        type: "output",
                                        text: request.text,
                                    });

                                    request.callback();

                                    break;
                                case "prompt":
                                    appendToOutput({
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
                                    appendToOutput({
                                        type: "choice",
                                        prompt: request.prompt,
                                        choices: request.choices,
                                        onSubmit: request.callback,
                                    });

                                    break;
                            }
                        });

                        setRunning(false);

                        if (!success) {
                            throw new Error("runner failed");
                        }
                    }
                } catch (error) {
                    console.error(error);

                    setOutput({
                        type: "error",
                        diagnostics:
                            "internal error: There was a problem running your code. Please reload the page and try again.",
                    });
                }
            }, 500),
        [props.id]
    );

    useEffect(() => {
        run(props.code, props.lint);
    }, [props.code, props.lint]);

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

    useEffect(() => {
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

    useEffect(() => {
        if (!isRunning) {
            document.getElementById(textAreaID)!.focus();
        }
    }, [isRunning]);

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
                        }}
                        value={props.code}
                        onValueChange={(code) => {
                            setHover(undefined);
                            props.onChange(code);
                        }}
                        highlight={(code) =>
                            prism.highlight(code, prism.languages.wipple, "wipple")
                        }
                        tabSize={2}
                        insertSpaces={false}
                        placeholder="Write your code here!"
                        autoFocus={props.autoFocus}
                    />

                    <animated.div style={animatedOutputStyle}>
                        <div
                            ref={outputRef}
                            className={(() => {
                                const successClasses =
                                    "p-4 bg-gray-50 dark:bg-gray-800 text-black dark:text-white";
                                const warningClasses =
                                    "p-4 bg-yellow-50 dark:bg-yellow-900 dark:opacity-50 text-yellow-900 dark:text-yellow-50";
                                const errorClasses =
                                    "p-4 bg-red-50 dark:bg-red-900 dark:opacity-50 text-red-900 dark:text-red-50";

                                if (output == null) {
                                    return "";
                                }

                                if (Array.isArray(output)) {
                                    return output.length ? successClasses : "";
                                } else {
                                    switch (output.type) {
                                        case "success":
                                            return successClasses;
                                        case "warning":
                                            return warningClasses;
                                        case "error":
                                            return errorClasses;
                                    }
                                }
                            })()}
                        >
                            {(() => {
                                if (output == null) {
                                    return null;
                                }

                                if (Array.isArray(output)) {
                                    return (
                                        <div>
                                            {output.map((item, index) => {
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
                                                }
                                            })}

                                            {isRunning ? (
                                                <div className="bouncing-loader">
                                                    <div></div>
                                                    <div></div>
                                                    <div></div>
                                                </div>
                                            ) : Array.isArray(output) &&
                                              output.find((item) => item.type !== "output") ? (
                                                <div className="mt-4">
                                                    <Button
                                                        variant="contained"
                                                        size="small"
                                                        endIcon={<Refresh />}
                                                        onClick={() => run(props.code, props.lint)}
                                                    >
                                                        Run again
                                                    </Button>
                                                </div>
                                            ) : null}
                                        </div>
                                    );
                                } else {
                                    switch (output.type) {
                                        case "success":
                                            return null;
                                        case "warning":
                                        case "error":
                                            return (
                                                <pre className="whitespace-pre-wrap break-words">
                                                    {output.diagnostics}
                                                </pre>
                                            );
                                    }
                                }
                            })()}
                        </div>
                    </animated.div>
                </div>

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
                    style={{ maxHeight: 300 }}
                >
                    <MenuList disablePadding>
                        {completions
                            .filter((c) => c.help)
                            .map((completion, index) => (
                                <MenuItem
                                    key={index}
                                    onClick={() => {
                                        const code = props.code;

                                        const before = code.slice(0, textEditor!.selectionEnd);
                                        const padBefore = (before[before.length - 1] ?? " ").match(
                                            /\s/
                                        )
                                            ? ""
                                            : " ";

                                        const after = code.slice(textEditor!.selectionEnd);
                                        const padAfter = (after[0] ?? " ").match(/\s/) ? "" : " ";

                                        props.onChange(
                                            before + padBefore + completion.name + padAfter + after
                                        );

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

                                        <ReactMarkdown
                                            remarkPlugins={[
                                                remarkMath,
                                                remarkGfm,
                                                remarkSmartypants,
                                            ]}
                                            rehypePlugins={[rehypeRaw, rehypeKatex]}
                                            linkTarget="_blank"
                                        >
                                            {completion.help}
                                        </ReactMarkdown>
                                    </ListItemText>
                                </MenuItem>
                            ))}
                    </MenuList>
                </Menu>
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