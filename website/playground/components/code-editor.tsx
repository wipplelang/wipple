import { useState } from "react";
import SimpleCodeEditor from "react-simple-code-editor";
import * as prism from "prismjs";
import { Output, useRunner } from "../runner";
import { useAsyncEffect } from "../helpers";

export interface CodeEditorProps {
    code: string;
    onChange: (code: string) => void;
}

export const CodeEditor = (props: CodeEditorProps) => {
    const [output, setOutput] = useState<Output | undefined>();

    const runner = useRunner();

    useAsyncEffect(async () => {
        const output = await runner.run(props.code);
        setOutput(output);
    }, [props.code]);

    return (
        <div className="bg-white dark:bg-gray-900 border-2 border-gray-100 dark:border-gray-700 rounded-lg overflow-clip">
            <SimpleCodeEditor
                className="m-4 dark:caret-white"
                textareaClassName="outline-0"
                preClassName="language-wipple"
                style={{
                    fontFamily: "'JetBrains Mono', monospace",
                    fontStyle: props.code ? "normal" : "italic",
                    fontVariantLigatures: "none",
                }}
                value={props.code}
                onValueChange={props.onChange}
                highlight={(code) => prism.highlight(code, prism.languages.wipple, "wipple")}
                tabSize={2}
                insertSpaces={false}
                placeholder="Write your code here!"
            />

            {output?.value ? (
                <div
                    className={`p-4 ${
                        output.success
                            ? "bg-gray-50 dark:bg-gray-800 text-black dark:text-white"
                            : "bg-red-50 dark:bg-red-900 dark:opacity-50 text-red-900 dark:text-red-50"
                    }`}
                >
                    <pre className="whitespace-pre-wrap break-word">{output.value}</pre>
                </div>
            ) : null}
        </div>
    );
};
