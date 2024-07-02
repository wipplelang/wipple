import ReactDOM from "react-dom/client";
import { useEffect, useState } from "react";
import { AlertProvider, Editor, Playground } from "wipple-playground";
import { useDebounceCallback } from "usehooks-ts";
import * as wipple from "wipple-wasm";
import "wipple-playground/dist/style.css";
import "./index.css";
import "./ios.css";

const getPlayground = () => fetch("/.bridge/playground").then((response) => response.json());

const updatePlayground = async (playground: Playground) => {
    const response = await fetch("/.bridge/updatePlayground", {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
        },
        body: JSON.stringify(playground),
    });

    if (!response.ok) {
        console.error("failed to save playground");
    }
};

const EditPage = () => {
    const [playground, setPlayground] = useState<Playground>();

    useEffect(() => {
        (async () => {
            console.log("fetching playground");

            const result = await getPlayground();
            if (!result) {
                console.error("no such playground");
                return;
            }

            setPlayground(result);
        })();
    }, []);

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

    const [selectedPageId, setSelectedPageId] = useState<string>();

    return (
        <Editor
            wipple={wipple}
            playground={playground}
            setPlayground={setPlayground}
            selectedPageId={selectedPageId}
            onSelectPage={setSelectedPageId}
        />
    );
};

ReactDOM.createRoot(document.getElementById("root")!).render(
    <AlertProvider>
        <EditPage />
    </AlertProvider>,
);
