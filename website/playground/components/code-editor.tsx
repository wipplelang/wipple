import { useId, useState } from "react";
import SimpleCodeEditor from "react-simple-code-editor";
import * as prism from "prismjs";
import ReactMarkdown from "react-markdown";
import { Diagnostic, Span, useRunner } from "../runner";
import { useAsyncEffect, useWindow } from "../helpers";

const overlaps = (span: Span, other: Span) => other.start >= span.start && other.end <= span.end;

interface ActiveDiagnostic {
    x: number;
    y: number;
    message: string;
    borderStyle: string;
}

export interface CodeEditorProps {
    code: string;
    onChange: (code: string) => void;
}

export const CodeEditor = (props: CodeEditorProps) => {
    const window = useWindow();

    const codeEditorID = useId();

    const [output, setOutput] = useState<[Span, string][]>([]);
    const [diagnostics, setDiagnostics] = useState<[DOMRect, Diagnostic][]>([]);
    const [activeDiagnostic, setActiveDiagnostic] = useState<ActiveDiagnostic | undefined>();

    const runner = useRunner();

    useAsyncEffect(async () => {
        const result = await runner.run(props.code);

        console.warn(result);

        const diagnostics: [DOMRect, Diagnostic][] = [];

        const $pre = document.getElementById(codeEditorID)?.querySelector("pre");
        if (!$pre) return;

        let offset = 0;
        for (const $el of $pre.childNodes as NodeListOf<HTMLElement>) {
            const length = $el.textContent!.length;

            const span = { start: offset, end: offset + length };

            for (const diagnostic of result.diagnostics) {
                if (overlaps(diagnostic.notes[0].span, span)) {
                    // These must be separate variables so Tailwind doesn't strip
                    // the styles out
                    let backgroundStyle: string;
                    let underlineStyle: string;
                    switch (diagnostic.level) {
                        case "Note":
                            backgroundStyle = "bg-gray-100";
                            underlineStyle = "decoration-gray-500";
                            break;
                        case "Warning":
                            backgroundStyle = "bg-yellow-100";
                            underlineStyle = "decoration-yellow-500";
                            break;
                        case "Error":
                            backgroundStyle = "bg-red-100";
                            underlineStyle = "decoration-red-500";
                            break;
                    }

                    $el.className = `${backgroundStyle} ${underlineStyle} underline decoration-y underline-offset-1 decoration-2`;

                    const rect = $el.getBoundingClientRect();
                    diagnostics.push([rect, diagnostic]);
                }
            }

            offset += length;
        }

        setOutput(result.output);
        setDiagnostics(diagnostics);
        setActiveDiagnostic(undefined);
    }, [props.code, window?.innerWidth]);

    return (
        <div className="bg-white border-2 border-gray-100 rounded-lg overflow-clip">
            <SimpleCodeEditor
                id={codeEditorID}
                className="m-4"
                textareaClassName="outline-0"
                preClassName="language-wipple"
                style={{
                    fontFamily: "'JetBrains Mono', monospace",
                    fontStyle: props.code ? "normal" : "italic",
                }}
                value={props.code}
                onValueChange={props.onChange}
                highlight={(code) => prism.highlight(code, prism.languages.wipple, "wipple")}
                tabSize={2}
                insertSpaces={false}
                placeholder="Write your code here!"
                onMouseMove={(e) => {
                    setActiveDiagnostic(undefined);

                    for (const [rect, diagnostic] of diagnostics) {
                        if (
                            rect.x <= e.clientX &&
                            e.clientX <= rect.x + rect.width &&
                            rect.y <= e.clientY &&
                            e.clientY <= rect.y + rect.height
                        ) {
                            let borderStyle: string;
                            switch (diagnostic.level) {
                                case "Note":
                                    borderStyle = "border-gray-400";
                                    break;
                                case "Warning":
                                    borderStyle = "border-yellow-400";
                                    break;
                                case "Error":
                                    borderStyle = "border-red-400";
                                    break;
                            }

                            setActiveDiagnostic({
                                x: rect.x,
                                y: rect.y,
                                borderStyle: borderStyle,
                                message: `**${diagnostic.message}**\n\n${diagnostic.notes[0].message}`,
                            });
                        }
                    }
                }}
                onMouseLeave={() => setActiveDiagnostic(undefined)}
            />

            {activeDiagnostic && (
                <div
                    className={`z-10 absolute p-2 bg-white border-2 rounded-md max-w-sm pointer-events-none ${activeDiagnostic.borderStyle}`}
                    style={{
                        top: `calc(${activeDiagnostic.y}px + 1.5em)`,
                        left: activeDiagnostic.x,
                        pointerEvents: "none",
                    }}
                >
                    <ReactMarkdown>{activeDiagnostic.message}</ReactMarkdown>
                </div>
            )}

            {output.length > 0 ? (
                <div className="bg-gray-50 p-4">
                    {output.map(([_span, text], index) => (
                        <pre key={index}>{text}</pre>
                    ))}
                </div>
            ) : null}
        </div>
    );
};
