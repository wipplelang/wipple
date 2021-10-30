import { useEffect, useRef, useState } from "react";
import SplitPane from "react-split-pane";
import Editor, { useMonaco } from "@monaco-editor/react";
import type monaco from "monaco-editor";
import { useRefState } from "../helpers/useRefState";

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

interface RunResult {
    output: string[];
    diagnostics: Diagnostic[];
}

const fontFamily = "'JetBrains Mono', monospace";

const Playground = () => {
    const [runner, setRunner] = useRefState<typeof import("../runner/pkg") | null>(null);
    useEffect(() => {
        (async () => {
            const runner = await import("../runner/pkg");
            setRunner(runner);
        })();
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

    const output = useRef<HTMLDivElement>(null);
    const [windowWidth, setWindowWidth] = useState(0);
    const [outputWidth, setOutputWidth] = useState(0);

    const updateWidths = () => {
        setWindowWidth(window.innerWidth);
        setOutputWidth(output.current?.clientWidth ?? 0);
    };

    useEffect(updateWidths, [output]);
    useEffect(() => window.addEventListener("resize", updateWidths), []);

    const editorWidth = windowWidth - outputWidth - 64;

    const [query, setQuery] = useState<URLSearchParams | undefined>();
    useEffect(() => setQuery(new URLSearchParams(window.location.search)), []);

    const monaco = useMonaco();
    const [model, setModel] = useRefState<monaco.editor.ITextModel | null>(null);

    const [result, setResult] = useState<RunResult | undefined>();

    const update = () => {
        if (!model.current) return;

        const code = model.current.getValue();

        if (query) {
            query.set("code", code);
            const newURL = window.location.pathname + "?" + query.toString();
            window.history.replaceState(null, "", newURL);
        }

        if (runner.current) {
            setResult(runner.current.run(code));
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
                    [/'/, "delimiter"],
                    [/[()\[\]{}]/, "@brackets"],
                    [/--.*/, "comment"],
                    [/"[^"\\]*(\\.[^"\\]*)*"/s, "string"],
                    [/[0-9]+(\.[0-9]+)?(?=[\n\t \(\)\[\]\{\}'"]|$)/, "number"],
                    [/[`~!@#$%^&*-_=+\\|;:,<.>/?]+(?=[\n\t \(\)\[\]\{\}'"]|$)/, "operator"],
                    [/[^\n\t \(\)\[\]\{\}'"]+/, "identifier"],
                ],
            },
        });

        monaco.editor.defineTheme("wipple", {
            base: "vs",
            inherit: true,
            rules: [
                { token: "comment", foreground: "#408080", fontStyle: "italic" },
                { token: "string", foreground: "#40a070" },
                { token: "number", foreground: "#40a070" },
                { token: "operator", foreground: "#0086b3" },
            ],
            colors: {},
        });

        monaco.editor.setTheme("wipple");

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

        const markers = result?.diagnostics.flatMap((diagnostic) => {
            diagnostic.notes[0].message = `${diagnostic.message}: ${diagnostic.notes[0].message}`;

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
    }, [model.current, result]);

    return (
        <div className="flex flex-col bg-gray-50" style={{ height: "100%" }}>
            <div className="flex items-center justify-between flex-grow-0 p-4 pb-0" ref={header}>
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

            <div className="flex-grow">
                <SplitPane
                    className="bg-gray-50"
                    style={{ height: windowHeight - headerHeight }}
                    primary="second"
                    defaultSize="40%"
                    resizerClassName="w-4"
                    resizerStyle={{ height: splitItemHeight }}
                    onChange={updateWidths}
                >
                    <div
                        className="m-4 mr-0 p-2 rounded-md bg-white z-50"
                        style={{ height: splitItemHeight }}
                    >
                        <Editor
                            width={editorWidth}
                            height="100%"
                            language="wipple"
                            defaultValue={query?.get("code") || "-- Write your code here!"}
                            options={{
                                fontFamily,
                                fontLigatures: true,
                                fontSize: 16,
                                minimap: {
                                    enabled: false,
                                },
                                tabSize: 2,
                                insertSpaces: false,
                            }}
                            onMount={initialize}
                        />
                    </div>

                    <div
                        ref={output}
                        className="m-4 ml-0 p-2 rounded-md bg-white overflow-scroll"
                        style={{ height: splitItemHeight, fontFamily }}
                    >
                        {result &&
                            result.output.map((line, index) => (
                                <pre className="whitespace-pre-wrap" key={index}>
                                    {line}
                                </pre>
                            ))}
                    </div>
                </SplitPane>
            </div>

            <div className="absolute bottom-0 left-0 right-0 flex-grow-0 p-4 text-center text-gray-400">
                <div ref={footer} className="-mb-2">
                    Built by{" "}
                    <a target="_blank" href="https://gramer.dev" className="text-gray-500">
                        Wilson Gramer
                    </a>
                </div>
            </div>
        </div>
    );
};

export default Playground;
