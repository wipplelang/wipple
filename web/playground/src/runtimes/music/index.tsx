import type { RuntimeComponent } from "..";
import { forwardRef, useImperativeHandle, useRef, useState } from "react";
import { PaletteItem } from "../../models";
import recordImage from "./record.svg";
import { Tooltip } from "../../components";
import { MaterialSymbol } from "react-material-symbols";
import { Player, instrument } from "soundfont-player";

type Status = "pending" | "playing" | "stopped";

export const Music: RuntimeComponent = forwardRef((props, ref) => {
    const containerRef = useRef<HTMLDivElement>(null);

    const [status, setStatus] = useState<Status>("pending");
    const [onPlay, setOnPlay] = useState(() => () => {});
    const audioContext = useRef<AudioContext>();
    const instruments = useRef<Record<string, Player>>({});

    const waitForPlay = async () => {
        if (status === "pending") {
            await new Promise<void>((resolve) => {
                setOnPlay(() => resolve);
            });
        }
    };

    const play = async () => {
        setStatus("playing");
        audioContext.current = new AudioContext();
        await audioContext.current.resume();
        onPlay();
    };

    const stop = () => {
        setStatus("stopped");
    };

    useImperativeHandle(ref, () => ({
        initialize: async () => {
            setStatus("pending");
        },
        onMessage: async (message, value) => {
            switch (message) {
                case "load-instrument": {
                    await waitForPlay();

                    if (!audioContext.current || instruments.current[value]) {
                        return;
                    }

                    const player = await instrument(audioContext.current, value);
                    instruments.current[value] = player;

                    break;
                }
                case "notes": {
                    const [instrumentName, msString, notes, callback] = value;

                    const ms = parseFloat(msString);

                    const player = instruments.current[instrumentName];
                    if (!player || !audioContext.current) {
                        return Promise.resolve(-1);
                    }

                    const startDate = new Date().valueOf();
                    const now = audioContext.current.currentTime;

                    for (const note of notes) {
                        player.play(note, now, { duration: ms / 1000 });
                    }

                    const delta = new Date().valueOf() - startDate;

                    setTimeout(() => {
                        if (!audioContext.current) {
                            props.call(callback, -1);
                            return;
                        }

                        props.call(callback, audioContext.current.currentTime - now);
                    }, ms - delta);

                    break;
                }
                default: {
                    throw new Error(`unsupported message: ${message}`);
                }
            }
        },
        cleanup: async () => {
            stop();
        },
    }));

    return (
        <div
            ref={containerRef}
            className="flex flex-row items-center justify-center w-fit p-2 gap-2 rounded-lg overflow-hidden border-2 border-gray-100 dark:border-gray-800"
        >
            <img
                src={recordImage}
                className={`w-20 h-20 ${
                    status === "playing" ? "animate-spin animate-duration-[3s]" : ""
                }`}
            />

            <div className="flex flex-row items-center gap-2">
                <Tooltip
                    disabled={status === "stopped"}
                    description={status === "playing" ? "Stop" : "Play"}
                >
                    <button
                        disabled={status === "stopped"}
                        className="flex items-center justify-center aspect-square p-1 disabled:opacity-50 enabled:hover:bg-gray-100 enabled:dark:hover:bg-gray-900 transition-colors border-2 border-gray-100 dark:border-gray-800 rounded-lg"
                        onClick={() => {
                            if (status === "playing") {
                                stop();
                                props.stopRunning();
                            } else {
                                play();
                            }
                        }}
                    >
                        <MaterialSymbol
                            icon={status === "playing" ? "stop" : "play_arrow"}
                            fill
                            size={18}
                        />
                    </button>
                </Tooltip>
            </div>
        </div>
    );
});

export const paletteItems: PaletteItem[] = [
    {
        title: "note",
        code: 'note [Note "C4"] ([Dropdown (1 / 8 , 1 / 4 , 1 / 2 , 1 , 2 , 3 , 4) 1] beats)',
    },
    {
        title: "chord",
        code: 'chord ([Note "C4"] , [Note "E4"] , [Note "G4"]) ([Dropdown (1 / 8 , 1 / 4 , 1 / 2 , 1 , 2 , 3 , 4) 1] beats)',
    },
    {
        title: "rest",
        code: "rest ([Dropdown (1 / 8 , 1 / 4 , 1 / 2 , 1 , 2 , 3 , 4) 1] beats)",
    },
    {
        title: "repeat",
        code: `repeat ([Dropdown (1 , 2 , 3 , 4 , 5 , 10 , 20 , 50 , 100) 1] times) {\n  _\n}`,
    },
];
