import { useState } from "react";
import { Button, Markdown } from "../components";
import { Help } from "../models";
import { MaterialSymbol } from "react-material-symbols";

export const HelpAlert = (props: { help: Help; dismiss: () => void }) => {
    const [showDeclaration, setShowDeclaration] = useState(false);

    return (
        <div className="w-[512px] help">
            <div className="flex flex-col w-full">
                <div className="flex flex-col">
                    <h1 className="text-2xl">
                        <code>{props.help.name}</code>

                        {props.help.declaration ? (
                            <button
                                onClick={() => setShowDeclaration(!showDeclaration)}
                                className={`inline-flex items-center justify-center h-4 rounded-md ml-2 align-middle mb-1 ${
                                    showDeclaration
                                        ? "bg-blue-500 dark:bg-blue-400"
                                        : "bg-gray-100 dark:bg-gray-800"
                                }`}
                            >
                                <MaterialSymbol
                                    icon="more_horiz"
                                    className={showDeclaration ? "text-white" : "text-gray-500"}
                                />
                            </button>
                        ) : null}
                    </h1>

                    <h2 className="text-gray-500 text-lg">
                        <Markdown>{props.help.summary}</Markdown>
                    </h2>

                    {props.help.example ? (
                        <div className="mt-2">
                            <a
                                href={props.help.example}
                                target="_blank"
                                className="bg-blue-500 bg-opacity-10 text-blue-500 dark:text-blue-400 hover:bg-opacity-100 hover:text-white rounded-md px-2 py-1 transition-colors"
                            >
                                Example &rarr;
                            </a>
                        </div>
                    ) : null}

                    {props.help.declaration && showDeclaration ? (
                        <div className="max-w-full text-sm">
                            <Markdown>{"```wipple\n" + props.help.declaration + "\n```"}</Markdown>
                        </div>
                    ) : null}
                </div>

                <div className="prose dark:prose-invert prose-blue prose-sm prose-code:text-sm prose-code:text-gray-900 dark:prose-code:text-gray-100">
                    <Markdown>{props.help.doc || "No additional documentation."}</Markdown>
                </div>

                <Button role="primary" fill onClick={props.dismiss}>
                    Done
                </Button>
            </div>
        </div>
    );
};
