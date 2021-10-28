import { useEffect, useLayoutEffect, useRef, useState } from "react";
import SplitPane from "react-split-pane";
import Editor, { useMonaco } from "@monaco-editor/react";
import monaco from "monaco-editor";
import type runner_ from "wipple_playground_runner";

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
    output: string;
    diagnostics: Diagnostic[];
}

const App = () => {
    let runner: typeof runner_ | undefined;
    import("wipple_playground_runner").then((r) => (runner = r));

    const header = useRef<HTMLDivElement>(null);
    const [windowHeight, setWindowHeight] = useState(0);
    const [headerHeight, setHeaderHeight] = useState(0);

    const updateHeights = () => {
        setWindowHeight(window.innerHeight);
        setHeaderHeight(header.current?.clientHeight ?? 0);
    };

    useLayoutEffect(updateHeights, [header]);
    window.addEventListener("resize", updateHeights);

    const splitViewHeight = windowHeight - headerHeight;
    const splitItemHeight = splitViewHeight - 32;

    const output = useRef<HTMLDivElement>(null);
    const [windowWidth, setWindowWidth] = useState(0);
    const [outputWidth, setOutputWidth] = useState(0);

    const updateWidths = () => {
        setWindowWidth(window.innerWidth);
        setOutputWidth(output.current?.clientWidth ?? 0);
    };

    useLayoutEffect(updateWidths, [output]);
    window.addEventListener("resize", updateWidths);

    const editorWidth = windowWidth - outputWidth - 64;

    const monaco = useMonaco();
    const [model, setModel] = useState<monaco.editor.ITextModel | null>(null);

    const [result, setResult] = useState<RunResult | undefined>();

    useEffect(() => {
        if (!monaco) return;

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

        const model = monaco.editor.getModels()[0];
        setModel(model);

        const debounce = (callback: () => void, wait: () => number) => {
            let timeout: NodeJS.Timeout;

            return () => {
                clearTimeout(timeout);
                timeout = setTimeout(callback, wait());
            };
        };

        const update = () => {
            const code = model.getValue();

            query.set("code", code);
            const newURL = window.location.pathname + "?" + query.toString();
            window.history.replaceState(null, "", newURL);

            if (!runner) return;

            setResult(runner.run(code));
        };

        model.onDidChangeContent(
            debounce(
                update,
                // Adjust the delay based on the size of the code (lines is a rough
                // but OK metric)
                () => Math.min(model.getValue().split("\n").length * 20, 1000)
            )
        );

        update();
    }, [monaco]);

    useEffect(() => {
        if (!monaco || !model) return;

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
                const startPos = model.getPositionAt(note.span.start);
                const endPos = model.getPositionAt(note.span.end);

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

        monaco.editor.setModelMarkers(model, "wipple", markers ?? []);
    }, [model, result]);

    const query = new URLSearchParams(window.location.search);

    return (
        <div className="flex flex-col bg-gray-50" style={{ height: "100%" }}>
            <div className="flex items-center justify-between flex-grow-0 p-4 pb-0" ref={header}>
                <a href="https://wipple.gramer.dev">
                    <img src="/images/logo.svg" alt="Wipple Playground" className="h-10" />
                </a>

                <div className="flex gap-4 text-gray-500">
                    <a href="https://docs.wipple.gramer.dev">Docs</a>
                    <a href="https://github.com/wipplelang/wipple">GitHub</a>
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
                >
                    <div
                        className="m-4 mr-0 p-2 rounded-md bg-white"
                        style={{ height: splitItemHeight }}
                    >
                        <Editor
                            width={editorWidth}
                            height="100%"
                            language="wipple"
                            defaultValue={query.get("code") || "-- Write your code here!"}
                            options={{
                                fontFamily: "JetBrains Mono",
                                fontLigatures: true,
                                fontSize: 16,
                                minimap: {
                                    enabled: false,
                                },
                                tabSize: 2,
                                insertSpaces: false,
                            }}
                        />
                    </div>

                    <div
                        ref={output}
                        className="m-4 ml-0 p-2 rounded-md bg-white"
                        style={{ height: splitItemHeight }}
                    >
                        {result?.output}
                    </div>
                </SplitPane>
            </div>
        </div>
    );
};

export default App;
