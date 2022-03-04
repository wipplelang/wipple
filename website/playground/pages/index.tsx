import theme from "../helpers/theme.json";
import { useEffect, useRef, useState } from "react";
import SplitPane from "react-split-pane";
import Editor, { useMonaco } from "@monaco-editor/react";
import type monaco from "monaco-editor";
import { useRefState } from "../helpers/useRefState";

interface RunResult {
    annotations: Annotation[];
    output: string[];
    diagnostics: Diagnostic[];
}

interface Annotation {
    span: Span;
    value: string;
}

interface Span {
    file: string;
    start: number;
    end: number;
}

interface Diagnostic {
    level: DiagnosticLevel;
    message: string;
    notes: Note[];
}

type DiagnosticLevel = "Note" | "Warning" | "Error";

interface Note {
    level: NoteLevel;
    span: Span;
    message: string;
}

type NoteLevel = "Primary" | "Secondary";

const fontFamily = "'JetBrains Mono', monospace";

const Playground = () => {
    const [runner, setRunner] = useRefState<Worker | undefined>(undefined);
    useEffect(() => {
        const runner = new Worker(new URL("../runner/worker.js", import.meta.url));
        setRunner(runner);
    }, []);

    const header = useRef<HTMLDivElement>(null);
    const footer = useRef<HTMLDivElement>(null);
    const [windowHeight, setWindowHeight] = useState(0);
    const [headerHeight, setHeaderHeight] = useState(0);
    const [footerHeight, setFooterHeight] = useState(0);

    const updateHeights = () => {
        setWindowHeight(window.innerHeight);
        setHeaderHeight(header.current?.clientHeight ?? 0);
        setFooterHeight(footer.current?.clientHeight ?? 0);
    };

    useEffect(updateHeights, [header]);
    useEffect(() => window.addEventListener("resize", updateHeights), []);

    const splitViewHeight = windowHeight - headerHeight - footerHeight;
    const splitItemHeight = splitViewHeight - 32;

    const outputEditor = useRef<HTMLDivElement>(null);
    const [windowWidth, setWindowWidth] = useState(0);
    const [outputWidth, setOutputWidth] = useState(0);

    const updateWidths = () => {
        setWindowWidth(window.innerWidth);
        setOutputWidth(outputEditor.current?.clientWidth ?? 0);
    };

    useEffect(updateWidths, [outputEditor]);
    useEffect(() => window.addEventListener("resize", updateWidths), []);

    const editorWidth = windowWidth - outputWidth - 64;

    const [query, setQuery] = useRefState<URLSearchParams | null>(null);
    useEffect(() => setQuery(new URLSearchParams(window.location.search)), []);

    const monaco = useMonaco();
    const [model, setModel] = useRefState<monaco.editor.ITextModel | null>(null);

    const [result, setResult] = useRefState<RunResult | null>(null);
    const [output, setOutput] = useState<string[]>([]);

    const update = () => {
        if (!model.current) return;

        const code = model.current.getValue();

        if (query.current) {
            query.current.set("code", code);
            const newURL = window.location.pathname + "?" + query.current.toString();
            window.history.replaceState(null, "", newURL);
        }

        if (runner.current) {
            runner.current.onmessage = (event) => {
                setResult(event.data);
                setOutput(event.data.output);
            };

            runner.current.postMessage(code);
        }
    };

    useEffect(update, [model.current, runner.current]);

    const initialize = (editor: monaco.editor.IEditor, monaco: typeof import("monaco-editor")) => {
        monaco.languages.register({ id: "wipple" });

        monaco.languages.setLanguageConfiguration("wipple", {
            wordPattern: /[^\n\t \(\)\[\]\{\}'"]+/,
            brackets: [
                ["(", ")"],
                ["[", "]"],
                ["{", "}"],
            ],
            autoClosingPairs: [
                { open: "(", close: ")" },
                { open: "[", close: "]" },
                { open: "{", close: "}" },
                { open: '"', close: '"' },
            ],
        });

        monaco.languages.setMonarchTokensProvider("wipple", {
            tokenizer: {
                root: [
                    [/\[:|:\]|[()\[\]{}]/, "@brackets"],
                    [/--.*/, "comment"],
                    [/:|::|->|=>/, "operator"],
                    [/['\/]/, "delimiter"],
                    [/_|use|when|type|trait|external/, "keyword"],
                    [/-?[0-9]+(\.[0-9]+)?/, "number"],
                    [/"[^"\\]*(?:\\.[^"\\]*)*"/s, "string"],
                    [/[A-Z][^\r\n\t \(\)\[\]\{\}'"/]*/, "type"],
                    [/[^\r\n\t \(\)\[\]\{\}'"/]+/, "name"],
                ],
            },
        });

        monaco.editor.defineTheme("wipple", theme as any);
        monaco.editor.setTheme("wipple");

        const getRange = (model: monaco.editor.ITextModel, span: Span) => {
            const startPos = model.getPositionAt(span.start);
            const endPos = model.getPositionAt(span.end);

            return new monaco.Range(
                startPos.lineNumber,
                startPos.column,
                endPos.lineNumber,
                endPos.column
            );
        };

        const getAnnotation = (model: monaco.editor.ITextModel, position: monaco.IPosition) => {
            const index = model.getOffsetAt(position);

            // Find the annotation with the smallest span covering the cursor
            return result.current?.annotations
                .filter(
                    (annotation) => index >= annotation.span.start && index <= annotation.span.end
                )
                .sort((a, b) => a.span.end - a.span.start - (b.span.end - b.span.start))[0];
        };

        monaco.languages.registerHoverProvider("wipple", {
            provideHover: (model, position) => {
                const annotation = getAnnotation(model, position);
                if (!annotation) return undefined;

                return {
                    range: getRange(model, annotation.span),
                    contents: [{ value: "```wipple\n" + annotation.value + "\n```" }],
                };
            },
        });

        const model = editor.getModel()! as monaco.editor.ITextModel;
        setModel(model);

        const debounce = (callback: () => void, wait: () => number) => {
            let timeout: NodeJS.Timeout;

            return () => {
                clearTimeout(timeout);
                timeout = setTimeout(callback, wait());
            };
        };

        model.onDidChangeContent(
            debounce(
                update,
                // Adjust the delay based on the size of the code (lines is a rough
                // but OK metric)
                () => Math.min(model.getValue().split("\n").length * 20, 1000)
            )
        );

        setTimeout(update, 500);
    };

    useEffect(() => {
        if (!monaco || !model.current) return;

        const severity = (level: NoteLevel, diagnosticLevel: DiagnosticLevel) => {
            switch (level) {
                case "Primary":
                    switch (diagnosticLevel) {
                        case "Note":
                            return monaco.MarkerSeverity.Hint;
                        case "Warning":
                            return monaco.MarkerSeverity.Warning;
                        case "Error":
                            return monaco.MarkerSeverity.Error;
                    }
                case "Secondary":
                    return monaco.MarkerSeverity.Hint;
            }
        };

        const markers = result.current?.diagnostics.flatMap((diagnostic) => {
            diagnostic.notes[0].message = `${diagnostic.message}\n${diagnostic.notes[0].message}`;

            return diagnostic.notes.map((note) => {
                const startPos = model.current!.getPositionAt(note.span.start);
                const endPos = model.current!.getPositionAt(note.span.end);

                return {
                    startLineNumber: startPos.lineNumber,
                    startColumn: startPos.column,
                    endLineNumber: endPos.lineNumber,
                    endColumn: endPos.column,
                    message: note.message,
                    severity: severity(note.level, diagnostic.level),
                };
            });
        });

        monaco.editor.setModelMarkers(model.current, "wipple", markers ?? []);
    }, [model.current, result.current]);

    return (
        <div className="bg-gray-50" style={{ height: "100%" }}>
            <div className="flex items-center justify-between p-4 pb-0" ref={header}>
                <a href="https://wipple.gramer.dev">
                    <img src="/images/logo.svg" alt="Wipple Playground" className="h-10" />
                </a>

                <div className="flex gap-4 text-gray-500">
                    <a target="_blank" href="https://docs.wipple.gramer.dev/learn">
                        Learn
                    </a>

                    <a target="_blank" href="https://docs.wipple.gramer.dev">
                        Docs
                    </a>

                    <a target="_blank" href="https://github.com/wipplelang/wipple">
                        GitHub
                    </a>
                </div>
            </div>

            <SplitPane
                className="bg-gray-50"
                style={{
                    top: headerHeight,
                    height: windowHeight - headerHeight,
                    overflow: "visible",
                }}
                primary="second"
                defaultSize="40%"
                resizerClassName="w-4"
                resizerStyle={{ height: splitItemHeight }}
                onChange={updateWidths}
            >
                <div
                    className="m-4 mr-0 p-2 rounded-md bg-white"
                    style={{ height: splitItemHeight }}
                >
                    <Editor
                        width={editorWidth}
                        height="100%"
                        language="wipple"
                        defaultValue={query.current?.get("code") || "-- Write your code here!"}
                        options={{
                            fontFamily,
                            fontLigatures: true,
                            fontSize: 16,
                            minimap: {
                                enabled: false,
                            },
                            tabSize: 2,
                            insertSpaces: false,
                            "semanticHighlighting.enabled": true,
                        }}
                        onMount={initialize}
                    />
                </div>

                <div
                    ref={outputEditor}
                    className="m-4 ml-0 p-2 rounded-md bg-white overflow-scroll"
                    style={{ height: splitItemHeight }}
                >
                    {output.map((line, index) => (
                        <pre className="whitespace-pre-wrap" key={index}>
                            {line}
                        </pre>
                    ))}
                </div>
            </SplitPane>

            <div className="absolute bottom-0 left-0 right-0 flex-grow-0 p-4 text-center text-gray-400">
                <div ref={footer} className="-mb-2">
                    Made by{" "}
                    <a target="_blank" href="https://gramer.dev" className="text-gray-500">
                        Wilson Gramer
                    </a>
                </div>
            </div>
        </div>
    );
};

export default Playground;
