import { useEffect, useRef, useState } from "react";
import { Player, instrument } from "soundfont-player";

export interface AppProps {
    setOnMessage: (handler: (message: string, value: any) => Promise<any>) => void;
}

export const App = (props: AppProps) => {
    const [handlePlay, setHandlePlay] = useState<(play: boolean) => void>();
    const audioContext = useRef<AudioContext>();
    const instruments = useRef<Record<string, Player>>({});
    const [isPlaying, setPlaying] = useState(false);

    useEffect(() => {
        (async () => {
            props.setOnMessage(async (message, value) => {
                switch (message) {
                    case "wait-for-play-button":
                        return new Promise<void>((resolve) => {
                            setHandlePlay(() => (play: boolean) => {
                                if (play) {
                                    audioContext.current = new AudioContext();
                                } else {
                                    audioContext.current = undefined;
                                }

                                setPlaying(play);
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
                        const [instrumentName, ms, notesList] = (value as string).split(" ");
                        const notes = notesList.split(",");
                        notes.splice(notes.length - 1, 1); // remove trailing comma

                        if (!audioContext.current) {
                            return;
                        }

                        const player = instruments.current[instrumentName];
                        if (!player) {
                            return;
                        }

                        for (const note of notes) {
                            player
                                .play(note, audioContext.current.currentTime)
                                .stop(audioContext.current.currentTime + parseFloat(ms));
                        }

                        return new Promise((resolve) => {
                            setTimeout(resolve, parseFloat(ms));
                        });
                    }
                    default:
                        throw new Error("unknown message");
                }
            });
        })();
    }, []);

    return (
        <div>
            <button onClick={() => handlePlay?.(!isPlaying)}>
                {isPlaying ? "⏹ Stop" : "▶️ Play"}
            </button>
        </div>
    );
};
