import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { useNavigate, useParams } from "react-router-dom";
import {
    Button,
    Editor,
    Playground,
    ShareHandlers,
    ShareId,
    shareIdsEqual,
    ShareProps,
    useAlert,
} from "wipple-playground";
import { getPlayground, updatePlayground } from "../../models";
import { produce } from "immer";
import QRCode from "react-qr-code";
import * as wipple from "wipple-wasm";
import { useDebounceCallback } from "usehooks-ts";
import { initializeSocketClient } from "../../helpers";
import { useStore } from "../../store";
import debounce from "debounce";

export const EditPage = () => {
    const params = useParams();
    const id = params.id!;
    const selectedPageId = params.page;

    const navigate = useNavigate();

    const [store, setStore] = useStore();
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

    const [isLoadingShare, setLoadingShare] = useState(false);

    const [sharedItemId, setSharedItemId] = useState<ShareId>();
    const sharedItemIdRef = useRef(sharedItemId);
    useEffect(() => {
        sharedItemIdRef.current = sharedItemId;
    }, [sharedItemId]);

    const [shareHandlers, setShareHandlers] = useState<ShareHandlers>();
    const shareHandlersRef = useRef(shareHandlers);
    useEffect(() => {
        shareHandlersRef.current = shareHandlers;
    }, [shareHandlers]);

    const share = useMemo((): ShareProps | undefined => {
        const { user, userInfo, offline } = store;

        if (!user || !userInfo?.classroomCode || offline) {
            return undefined;
        }

        return {
            id: sharedItemId,
            isLoading: isLoadingShare,
            shareHandlers,
            onToggle: async (shareId) => {
                const disconnect = () => {
                    shareHandlersRef.current?.stop();
                    setSharedItemId(undefined);
                    setShareHandlers(undefined);
                };

                disconnect();

                if (sharedItemIdRef.current && shareIdsEqual(sharedItemIdRef.current, shareId)) {
                    return;
                }

                setLoadingShare(true);
                try {
                    const socketClient = await initializeSocketClient(user, {
                        onDisconnect: disconnect,
                    });

                    if (!socketClient) {
                        alert("Couldn't connect to teacher; please try again.");
                        return;
                    }

                    setSharedItemId(shareId);

                    setShareHandlers({
                        update: debounce((item) => {
                            socketClient.update(item);
                        }, 250),
                        askForHelp: () => {
                            socketClient.askForHelp();
                        },
                        stop: () => {
                            socketClient.disconnect();
                        },
                    });
                } finally {
                    setLoadingShare(false);
                }
            },
        };
    }, [store.user, store.offline, isLoadingShare, sharedItemId, shareHandlers]);

    useEffect(() => {
        if (playground) {
            savePlayground(playground);

            if (shareHandlers && sharedItemId) {
                const page = playground.pages.find((page) => page.id === sharedItemId.page);
                const item = page?.items[sharedItemId.index];

                if (item) {
                    shareHandlers.update(item);
                }
            }
        }
    }, [playground, shareHandlers, sharedItemId]);

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
            share={share}
        />
    );
};
