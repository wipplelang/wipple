import { useEffect, useMemo, useState } from "react";
import SimpleCodeEditor from "react-simple-code-editor";
import * as prism from "prismjs";
import { debounce, useMediaQuery } from "@mui/material";
import { Globals as SpringGlobals, useSpring, animated } from "react-spring";
import useMeasure from "react-use-measure";
import { Output, useRunner } from "../runner";
import { useAsyncEffect } from "../helpers";

export interface CodeEditorProps {
    code: string;
    lint: boolean;
    autoFocus: boolean;
    onChange: (code: string) => void;
}

export const CodeEditor = (props: CodeEditorProps) => {
    const [output, setOutput] = useState<Output | undefined>();

    const [outputRef, { height: outputHeight }] = useMeasure();
    const animatedOutputStyle = useSpring(
        output?.value ? { opacity: 1, height: outputHeight } : { opacity: 0, height: 0 }
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
                const output = await runner.run(code, lint);
                setOutput(output);
            }, 750),
        []
    );

    useAsyncEffect(() => run(props.code, props.lint), [props.code, props.lint]);

    return (
        <div className="bg-white dark:bg-gray-900 border-2 border-gray-100 dark:border-gray-700 rounded-lg overflow-clip">
            <SimpleCodeEditor
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
                onValueChange={props.onChange}
                highlight={(code) => prism.highlight(code, prism.languages.wipple, "wipple")}
                tabSize={2}
                insertSpaces={false}
                placeholder="Write your code here!"
                autoFocus={props.autoFocus}
            />

            <animated.div style={animatedOutputStyle}>
                <div
                    ref={outputRef}
                    className={`p-4 ${(() => {
                        const successClasses =
                            "bg-gray-50 dark:bg-gray-800 text-black dark:text-white";
                        const warningClasses =
                            "bg-yellow-50 dark:bg-yellow-900 dark:opacity-50 text-yellow-900 dark:text-yellow-50";
                        const errorClasses =
                            "bg-red-50 dark:bg-red-900 dark:opacity-50 text-red-900 dark:text-red-50";

                        switch (output?.status) {
                            case "success":
                                return successClasses;
                            case "warning":
                                return warningClasses;
                            case "error":
                                return errorClasses;
                            default:
                                return "";
                        }
                    })()}`}
                >
                    <pre className="whitespace-pre-wrap break-words">{output?.value}</pre>
                </div>
            </animated.div>
        </div>
    );
};
