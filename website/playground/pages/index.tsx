import theme from "../helpers/theme.json";
import { useEffect, useRef, useState } from "react";
import Editor, { Monaco, useMonaco } from "@monaco-editor/react";
import type monaco from "monaco-editor";
import { useRefState } from "../helpers/useRefState";

interface RunResult {
    annotations: Annotation[];
    output: [Span, string][];
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

interface Completion {
    name: string;
    kind: number;
}

const fontFamily = "'JetBrains Mono', monospace";

let result: RunResult | undefined;

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

    const editorHeight = windowHeight - headerHeight - footerHeight - 48;

    useEffect(updateHeights, [header, footer]);
    useEffect(() => window.addEventListener("resize", updateHeights), []);

    const [windowWidth, setWindowWidth] = useState(0);

    const updateWidths = () => {
        setWindowWidth(window.innerWidth);
    };

    useEffect(updateWidths, []);
    useEffect(() => window.addEventListener("resize", updateWidths), []);

    const editorWidth = windowWidth - 32;

    const monaco = useMonaco();
    const [model, setModel] = useRefState<monaco.editor.ITextModel | null>(null);

    const [query, setQuery] = useRefState<URLSearchParams | null>(null);
    useEffect(() => setQuery(new URLSearchParams(window.location.search)), []);

    const [updateTrigger, triggerUpdate] = useState({});
    const [decorationIDs, setDecorationIDs] = useRefState<string[]>([]);

    const getRange = (m: Monaco, model: monaco.editor.ITextModel, span: Span) => {
        const startPos = model.getPositionAt(span.start);
        const endPos = model.getPositionAt(span.end);

        return new m.Range(startPos.lineNumber, startPos.column, endPos.lineNumber, endPos.column);
    };

    const updateDecorations = (monaco: Monaco, model: monaco.editor.ITextModel) => {
        const newIDs = model.deltaDecorations(
            decorationIDs.current,
            result?.output.map(([span, output], index) => {
                const range = getRange(monaco, model, span);
                range.setStartPosition(range.endLineNumber, range.endColumn + 2);
                range.setEndPosition(range.endLineNumber, range.endColumn + 2);

                const decoration: monaco.editor.IModelDecoration = {
                    ownerId: 0,
                    id: index.toString(),
                    range,
                    options: {
                        after: {
                            content: output,
                            inlineClassName: "editor-output-decoration",
                        },
                    },
                };

                return decoration;
            }) ?? []
        );

        setDecorationIDs(newIDs);
    };

    const update = () => {
        if (!model.current) return;

        const code = model.current.getValue();

        query.current!.set("code", code);
        const newURL = window.location.pathname + (code ? "?" + query.current!.toString() : "");
        window.history.replaceState(null, "", newURL);

        if (runner.current) {
            runner.current.onmessage = (event) => {
                result = event.data;
                triggerUpdate({});
            };

            runner.current.postMessage({ type: "run", code });
        }
    };

    useEffect(update, [model.current, runner.current]);

    const initialize = (
        editor: monaco.editor.IStandaloneCodeEditor,
        monaco: typeof import("monaco-editor")
    ) => {
        editor.focus();

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
                    [/_|use|when|type|trait|instance|where|external/, "keyword"],
                    [/-?[0-9]+(\.[0-9]+)?/, "number"],
                    [/"[^"\\]*(?:\\.[^"\\]*)*"/s, "string"],
                    [/[A-Z][^\r\n\t \(\)\[\]\{\}'"/]*/, "type"],
                    [/[^\r\n\t \(\)\[\]\{\}'"/]+/, "name"],
                ],
            },
        });

        monaco.editor.defineTheme("wipple", theme as any);
        monaco.editor.setTheme("wipple");

        const getAnnotation = (model: monaco.editor.ITextModel, position: monaco.IPosition) => {
            const index = model.getOffsetAt(position);

            // Find the annotation with the smallest span covering the cursor
            return result?.annotations
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
                    range: getRange(monaco, model, annotation.span),
                    contents: [{ value: "```wipple\n" + annotation.value + "\n```" }],
                };
            },
        });

        monaco.languages.registerCompletionItemProvider("wipple", {
            provideCompletionItems: async (model, position) => {
                if (!runner.current) {
                    return { suggestions: [] };
                }

                const completions: Completion[] = await new Promise((resolve) => {
                    runner.current!.onmessage = (event) => {
                        resolve(event.data);
                    };

                    runner.current!.postMessage({
                        type: "completions",
                        position: model.getOffsetAt(position),
                    });
                });

                return {
                    suggestions: completions.map(
                        (completion) =>
                            ({
                                label: completion.name,
                                kind: completion.kind,
                                insertText: completion.name,
                            } as monaco.languages.CompletionItem)
                    ),
                };
            },
        });

        editor.onDidChangeCursorSelection((event) => {
            if (event.selection.isEmpty()) {
                updateDecorations(monaco, model);
            } else {
                model.deltaDecorations(decorationIDs.current, []);
            }
        });

        const model = editor.getModel()!;
        setModel(model);

        model.onDidChangeContent(update);
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

        const markers =
            result?.diagnostics?.flatMap((diagnostic) => {
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
            }) ?? [];

        monaco.editor.setModelMarkers(model.current, "wipple", markers);

        updateDecorations(monaco, model.current);
    }, [updateTrigger]);

    return (
        <>
            <div className="bg-gray-50" style={{ height: windowHeight }}>
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

                <div
                    className="m-4 mr-0 p-2 rounded-md bg-white"
                    style={{
                        top: headerHeight,
                        width: editorWidth,
                        overflow: "visible",
                    }}
                >
                    {query.current && (
                        <div className="relative w-full">
                            <Editor
                                width="100%"
                                className="absolute inset-0"
                                height={editorHeight}
                                language="wipple"
                                defaultValue={query.current.get("code") ?? undefined}
                                options={{
                                    fontFamily,
                                    fontLigatures: true,
                                    fontSize: 16,
                                    minimap: {
                                        enabled: false,
                                    },
                                    tabSize: 2,
                                    "semanticHighlighting.enabled": true,
                                }}
                                onMount={initialize}
                            />

                            {!query.current.get("code") && (
                                <div
                                    className="absolute inset-0 pointer-events-none"
                                    style={{
                                        fontFamily,
                                        fontStyle: "italic",
                                        color: theme.rules[0].foreground,
                                        marginLeft: 76,
                                    }}
                                >
                                    Write your code here!
                                </div>
                            )}
                        </div>
                    )}
                </div>

                <div className="absolute bottom-0 left-0 right-0 flex-grow-0 p-4 text-center text-gray-400">
                    <div ref={footer} className="-mb-2">
                        Made by{" "}
                        <a target="_blank" href="https://gramer.dev" className="text-gray-500">
                            Wilson Gramer
                        </a>
                    </div>
                </div>
            </div>
        </>
    );
};

export default Playground;
