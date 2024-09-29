import { useCallback, useEffect, useState } from "react";
import { ContextMenuButton, Navbar } from "../../components";
import { getPlayground, Playground, updatePlayground } from "../../models";
import { produce } from "immer";
import { CodeEditor } from "./code-editor";
import { useParams } from "react-router-dom";
import { useDebounceCallback } from "usehooks-ts";
import * as wipple from "wipple-wasm";

export const EditPage = () => {
    const params = useParams();
    const id = params.id!;

    const [playground, setPlayground] = useState<Playground>();

    useEffect(() => {
        (async () => {
            const result = await getPlayground(id);
            if (!result) {
                console.error(`no such playground ${id}`);
                // TODO: Show 404 page
                return;
            }

            setPlayground(result);
        })();
    }, [id]);

    const savePlayground = useDebounceCallback(async (playground: Playground) => {
        await updatePlayground(playground);
    }, 1000);

    useEffect(() => {
        if (playground) {
            savePlayground(playground);
        }
    }, [playground]);

    useEffect(() => {
        const handler = () => {
            if (playground) {
                updatePlayground(playground);
            }
        };

        window.addEventListener("beforeunload", handler);

        return () => {
            window.removeEventListener("beforeunload", handler);
        };
    }, [playground]);

    const rename = useCallback(() => {
        if (!playground) return;

        const name = prompt("What do you want to call your playground?", playground.name);
        if (name) {
            setPlayground(
                produce((playground) => {
                    if (!playground) return;
                    playground.name = name;
                }),
            );
        }
    }, [playground]);

    const exportAsJson = useCallback(() => {
        if (!playground) return;

        (async () => {
            const json = JSON.stringify(playground, null, 4);
            await navigator.clipboard.writeText(json);
            alert("JSON copied to clipboard.");
        })();
    }, [playground]);

    return (
        <div className="flex flex-col h-screen">
            <Navbar
                title={
                    playground?.name ? (
                        <ContextMenuButton
                            className="flex flex-col items-start -mx-2 -my-1 px-2 p-1 rounded-lg transition hover:bg-gray-200 dark:hover:bg-gray-800"
                            items={[
                                {
                                    title: "Rename",
                                    icon: "edit",
                                    onClick: rename,
                                },
                                {
                                    title: "Export as JSON",
                                    icon: "data_object",
                                    onClick: exportAsJson,
                                },
                            ]}
                        >
                            <p className="font-semibold">{playground.name}</p>
                            <p className="text-xs font-semibold opacity-40 -mt-0.5 capitalize">
                                {playground.setup}
                            </p>
                        </ContextMenuButton>
                    ) : null
                }
            />

            <div className="relative flex-1 w-full h-full">
                <div className="absolute inset-0 overflow-visible md:overflow-clip">
                    {playground ? (
                        <CodeEditor
                            wipple={wipple}
                            onChange={(code) =>
                                setPlayground(
                                    produce((playground) => {
                                        if (playground) {
                                            playground.code = code;
                                        }
                                    }),
                                )
                            }
                            runtime={playground.setup ?? undefined}
                        >
                            {playground.code}
                        </CodeEditor>
                    ) : null}
                </div>
            </div>
        </div>
    );
};
