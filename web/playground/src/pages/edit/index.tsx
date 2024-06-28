import { useCallback, useEffect, useState } from "react";
import { useLocation, useNavigate, useParams } from "react-router-dom";
import { Button, Editor, Playground, useAlert } from "wipple-playground";
import { getPlayground, updatePlayground } from "../../models";
import { produce } from "immer";
import QRCode from "react-qr-code";
import * as wipple from "wipple-wasm";
import { useDebounceCallback } from "usehooks-ts";

export const EditPage = () => {
    const params = useParams();
    const id = params.id!;
    const selectedPageId = params.page;

    const navigate = useNavigate();

    const { displayAlert } = useAlert();

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

    const makePublic = useCallback(() => {
        if (!playground) return;

        setPlayground(
            produce((playground) => {
                if (!playground) return;

                if (!playground.collaborators.includes("*")) {
                    playground.collaborators.push("*");
                }
            }),
        );

        const playgroundId = playground?.id;

        displayAlert(({ dismiss }) => (
            <div className="flex flex-col items-stretch justify-center gap-4 p-2 text-center">
                <p>
                    Playground is public!
                    <br />
                    Share it with this QR code:
                </p>

                <QRCode value={`${window.origin}/playground/edit/${playgroundId}`} />

                <Button role="primary" fill onClick={dismiss}>
                    Done
                </Button>
            </div>
        ));
    }, [playground]);

    return (
        <Editor
            wipple={wipple}
            playground={playground}
            setPlayground={setPlayground}
            selectedPageId={selectedPageId}
            onSelectPage={(id) => {
                navigate(`/playground/edit/${playground!.id}/${id}`);
            }}
            onMakePublic={makePublic}
        />
    );
};
