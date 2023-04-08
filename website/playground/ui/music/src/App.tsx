import { useEffect, useRef, useState } from "react";
import { Player, instrument } from "soundfont-player";

export interface AppProps {
    setOnMessage: (handler: (message: string, value: any) => Promise<any>) => void;
}

type Status = "pending" | "playing" | "stopped";

export const App = (props: AppProps) => {
    const [handleStatus, setHandleStatus] = useState<(status: Status) => void>();
    const audioContext = useRef<AudioContext>();
    const instruments = useRef<Record<string, Player>>({});
    const [status, setStatus] = useState<Status>("pending");

    useEffect(() => {
        (async () => {
            props.setOnMessage(async (message, value) => {
                switch (message) {
                    case "wait-for-play-button":
                        return new Promise<void>((resolve) => {
                            setHandleStatus(() => (status: Status) => {
                                if (status === "playing") {
                                    audioContext.current = new AudioContext();
                                } else {
                                    audioContext.current = undefined;
                                }

                                setStatus(status);
                                resolve();
                            });
                        });
                    case "load-instrument": {
                        if (!audioContext.current || instruments.current[value]) {
                            return;
                        }

                        const player = await instrument(audioContext.current, value);
                        instruments.current[value] = player;

                        break;
                    }
                    case "notes": {
                        const [instrumentName, msString, notesList] = (value as string).split(" ");
                        const notes = notesList.split(",");
                        notes.splice(notes.length - 1, 1); // remove trailing comma

                        const ms = parseFloat(msString);

                        const player = instruments.current[instrumentName];
                        if (!player) {
                            return;
                        }

                        if (!audioContext.current) {
                            return Promise.resolve(-1);
                        }

                        const startDate = new Date().valueOf();
                        const now = audioContext.current.currentTime;

                        for (const note of notes) {
                            player.play(note, now, { duration: ms / 1000 });
                        }

                        return new Promise<number>((resolve) => {
                            const delta = new Date().valueOf() - startDate;

                            setTimeout(() => {
                                if (!audioContext.current) {
                                    resolve(-1);
                                    return;
                                }

                                resolve(audioContext.current.currentTime - now);
                            }, ms - delta);
                        });
                    }
                    case "done":
                        setStatus("stopped");
                        break;
                    default:
                        throw new Error("unknown message");
                }
            });
        })();
    }, []);

    return (
        <div>
            {status !== "stopped" ? (
                <button
                    onClick={() => {
                        if (!handleStatus) {
                            return;
                        }

                        switch (status) {
                            case "pending":
                                handleStatus("playing");
                                break;
                            case "playing":
                                handleStatus("stopped");
                                break;
                        }
                    }}
                >
                    {status === "playing" ? "⏹ Stop" : "▶️ Play"}
                </button>
            ) : null}
        </div>
    );
};
