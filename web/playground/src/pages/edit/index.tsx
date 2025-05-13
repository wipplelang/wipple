import { useCallback, useEffect, useState } from "react";
import { getPlayground, Playground, updatePlayground } from "../../models";
import { produce } from "immer";
import { CodeEditor } from "./code-editor";
import { useNavigate, useParams } from "react-router-dom";
import { useDebounceCallback } from "usehooks-ts";
import * as wipple from "wipple-wasm";

export const EditPage = () => {
    const params = useParams();
    const id = params.id!;

    const navigate = useNavigate();

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

    const handleNewPlayground = useCallback(() => {
        const confirmed = confirm("Creating a new playground will clear this one. Are you sure?");
        if (!confirmed) return;

        navigate(import.meta.env.BASE_URL);
    }, []);

    const handleChange = useCallback(
        (code: string) =>
            setPlayground(
                produce((playground) => {
                    if (playground) {
                        playground.code = code;
                    }
                }),
            ),
        [],
    );

    return (
        <div className="flex flex-col h-screen">
            <div className="relative flex-1 w-full h-full">
                <div className="absolute inset-0 overflow-clip">
                    {playground ? (
                        <CodeEditor
                            wipple={wipple}
                            onNewPlayground={handleNewPlayground}
                            onChange={handleChange}
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
