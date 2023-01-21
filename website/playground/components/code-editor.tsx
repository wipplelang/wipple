import { useEffect, useMemo, useState } from "react";
import SimpleCodeEditor from "react-simple-code-editor";
import * as prism from "prismjs";
import { debounce, useMediaQuery } from "@mui/material";
import { Globals as SpringGlobals, useSpring, animated } from "react-spring";
import useMeasure from "react-use-measure";
import {
    AnalysisOutput,
    AnalysisOutputDiagnostics,
    AnalysisOutputSyntaxHighlightingItem,
    HoverOutput,
    useRunner,
} from "../runner";
import { useAsyncEffect } from "../helpers";

export interface CodeEditorProps {
    id: string;
    code: string;
    lint: boolean;
    autoFocus: boolean;
    onChange: (code: string) => void;
}

interface Hover {
    x: number;
    y: number;
    output: HoverOutput;
}

export const CodeEditor = (props: CodeEditorProps) => {
    const editorID = `code-editor-${props.id}`;
    const textAreaID = `code-editor-text-${props.id}`;

    const [syntaxHighlighting, setSyntaxHighlighting] = useState<
        AnalysisOutputSyntaxHighlightingItem[]
    >([]);

    const [output, setOutput_] = useState<AnalysisOutputDiagnostics | string | undefined>();
    const setOutput = debounce(setOutput_, 100);

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
                    const analysis = await runner.analyze(props.id, code, lint);
                    setSyntaxHighlighting(analysis.syntaxHighlighting);
                    setOutput(analysis.diagnostics);

                    if (analysis.diagnostics.type !== "error") {
                        const output = await runner.run(props.id);
                        setOutput(output);
                    }
                } catch (error) {
                    console.error(error);

                    setOutput({
                        type: "error",
                        diagnostics: `internal error: There was a problem running your code. Please reload the page and try again.`,
                    });
                }
            }, 500),
        [props.id]
    );

    useAsyncEffect(() => run(props.code, props.lint), [props.code, props.lint]);

    const [hover, setHover_] = useState<Hover>();
    const setHover = useMemo(() => debounce(setHover_, 250), []);
    const clearHover = () => setHover_(undefined);

    const textEditor = document.getElementById(textAreaID) as HTMLTextAreaElement | null;

    const [mousePosition, setMousePosition] = useState<[number, number]>();

    useEffect(() => {
        const listener = (e: MouseEvent) => {
            setMousePosition([e.x, e.y]);
        };

        window.addEventListener("click", listener);

        return () => {
            window.removeEventListener("click", listener);
        };
    }, [setMousePosition]);

    useAsyncEffect(async () => {
        if (!textEditor || !mousePosition) return;

        const [mouseX, mouseY] = mousePosition;
        const textEditorRect = textEditor.getBoundingClientRect();
        if (
            mouseX < textEditorRect.left ||
            mouseX >= textEditorRect.right ||
            mouseY < textEditorRect.top ||
            mouseY >= textEditorRect.bottom
        ) {
            clearHover();
            return;
        }

        if (textEditor.selectionEnd - textEditor.selectionStart === 0) {
            clearHover();
            return;
        }

        const output = await runner.hover(
            props.id,
            textEditor.selectionStart,
            textEditor.selectionEnd
        );

        if (!output) {
            clearHover();
            return;
        }

        let hoverElement: HTMLElement | undefined;
        for (const el of document.querySelectorAll<HTMLSpanElement>(`#${editorID} .token`)) {
            const rect = el.getBoundingClientRect();

            if (
                mouseX >= rect.left &&
                mouseX < rect.right &&
                mouseY >= rect.top &&
                mouseY < rect.bottom
            ) {
                hoverElement = el;
                break;
            }
        }

        if (!hoverElement) {
            clearHover();
            return;
        }

        setHover({
            x: hoverElement.getBoundingClientRect().x,
            y: hoverElement.getBoundingClientRect().bottom + window.scrollY,
            output,
        });
    }, [props.id, mousePosition]);

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
        }
    }, [syntaxHighlighting]);

    return (
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
                    clearHover();
                    props.onChange(code);
                }}
                highlight={(code) => prism.highlight(code, prism.languages.wipple, "wipple")}
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

                        switch (typeof output) {
                            case "string":
                                return output.length ? successClasses : "";
                            default:
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
                    <pre className="whitespace-pre-wrap break-words">
                        {(() => {
                            if (output == null) {
                                return null;
                            }

                            switch (typeof output) {
                                case "string":
                                    return output;
                                default:
                                    switch (output.type) {
                                        case "success":
                                            return "Running..."; // TODO: Replace with animated spinner
                                        case "warning":
                                        case "error":
                                            return output.diagnostics;
                                    }
                            }
                        })()}
                    </pre>
                </div>
            </animated.div>

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
