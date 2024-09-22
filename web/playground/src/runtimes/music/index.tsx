import type { RuntimeComponent } from "..";
import { forwardRef, useCallback, useImperativeHandle, useRef, useState } from "react";
import { PaletteCategory } from "../../models";
import { CacheStorage, DrumMachine, Soundfont, SplendidGrandPiano } from "smplr";
import { decodeMelody, decodeRhythm } from "../../edit/melody-picker";
import { MaterialSymbol } from "react-material-symbols";
import { Tooltip } from "../../components";
import { Mutex } from "async-mutex";

export interface Settings {}

const cache = new CacheStorage();

const audioContextMutex = new Mutex();
const audioContext = new AudioContext();
export const getAudioContext = () =>
    audioContextMutex.runExclusive(async () => {
        if (audioContext.state === "suspended") {
            console.log("Waiting for user interaction to resume audio context...");

            await new Promise<void>((resolve) => {
                const handleClick = async () => {
                    console.log("Resuming audio context...");
                    await audioContext.resume();
                    console.log("Audio context resumed");

                    window.removeEventListener("click", handleClick);

                    resolve();
                };

                window.addEventListener("click", handleClick);
            });
        }

        return audioContext;
    });

const pianoMutex = new Mutex();
let piano: SplendidGrandPiano | undefined;
export const getPiano = () =>
    pianoMutex.runExclusive(async () => {
        if (piano) {
            return piano;
        }

        const audioContext = await getAudioContext();
        piano = await new SplendidGrandPiano(audioContext, { storage: cache }).load;
        return piano;
    });

const drumMachineMutex = new Mutex();
let drumMachine: DrumMachine | undefined;
export const getDrumMachine = () =>
    drumMachineMutex.runExclusive(async () => {
        if (drumMachine) {
            return drumMachine;
        }

        const audioContext = await getAudioContext();

        drumMachine = await new DrumMachine(audioContext, {
            instrument: "Casio-RZ1",
            storage: cache,
        }).load;

        return drumMachine;
    });

export const soundfontInstrumentNames: Record<string, string> = {
    "electric-guitar": "electric_guitar_clean",
    flute: "flute",
    harmonica: "harmonica",
    marimba: "marimba",
    "orchestra-hit": "orchestra_hit",
    strings: "string_ensemble_1",
    trumpet: "trumpet",
    tuba: "tuba",
    violin: "violin",
};

const soundfontInstruments = Object.fromEntries(
    Object.keys(soundfontInstrumentNames).map((name) => [
        name,
        {
            mutex: new Mutex(),
            instrument: undefined as Soundfont | undefined,
        },
    ]),
);

export const getSoundfontInstrument = async (name: string): Promise<Soundfont | undefined> =>
    await soundfontInstruments[name]?.mutex.runExclusive(async () => {
        if (soundfontInstruments[name].instrument) {
            return soundfontInstruments[name].instrument;
        }

        const audioContext = await getAudioContext();

        const instrument = await new Soundfont(audioContext, {
            instrument: soundfontInstrumentNames[name],
            storage: cache,
        }).load;

        soundfontInstruments[name].instrument = instrument;

        return instrument;
    });

export const stopAllInstruments = () => {
    piano?.stop();

    for (const instrument of Object.values(soundfontInstruments)) {
        instrument.instrument?.stop();
    }
};

export const defaultTempo = 120;

export const Music: RuntimeComponent<Settings> = forwardRef((props, ref) => {
    const [isRunning, setRunning] = useState(false);

    const togetherRef = useRef<number>();
    const tempoRef = useRef(defaultTempo);

    const waitForTogether = useCallback(async () => {
        const duration = togetherRef.current;
        if (duration == null) {
            return;
        }

        await new Promise((resolve) => setTimeout(resolve, duration * 1000));

        togetherRef.current = undefined;
    }, []);

    useImperativeHandle(ref, () => ({
        initialize: async () => {
            await Promise.all([
                getPiano(),
                ...Object.values(soundfontInstrumentNames).map(getSoundfontInstrument),
            ]);

            stopAllInstruments();

            togetherRef.current = undefined;
            tempoRef.current = defaultTempo;

            setRunning(true);
        },
        onMessage: async (message, value) => {
            switch (message) {
                case "tempo": {
                    tempoRef.current = value;
                    break;
                }
                case "begin-together": {
                    togetherRef.current = 0;
                    break;
                }
                case "end-together": {
                    await waitForTogether();
                    break;
                }
                case "play": {
                    const [instrumentName, kind, encoded] = value;

                    let instrument: SplendidGrandPiano | Soundfont | DrumMachine | undefined;
                    let sequence: string[][];
                    switch (kind) {
                        case "melody": {
                            switch (instrumentName) {
                                case "piano":
                                    instrument = await getPiano();
                                    break;
                                case "drums": {
                                    throw new Error("expected instrument, not drums");
                                }
                                default:
                                    instrument = await getSoundfontInstrument(instrumentName);
                                    break;
                            }

                            if (!instrument) {
                                throw new Error(`unknown instrument: ${instrumentName}`);
                            }

                            sequence = decodeMelody(encoded).notes;

                            break;
                        }
                        case "rhythm": {
                            if (instrumentName !== "drums") {
                                throw new Error("expected drums");
                            }

                            instrument = await getDrumMachine();

                            sequence = decodeRhythm(encoded).notes;

                            break;
                        }
                        default: {
                            throw new Error(`unsupported kind: ${kind}`);
                        }
                    }

                    const noteDuration = 60 / tempoRef.current;
                    const totalDuration = noteDuration * sequence.length;

                    let time = audioContext.currentTime;
                    for (const notes of sequence) {
                        for (const note of notes) {
                            instrument.start({ note, time, duration: noteDuration });
                        }

                        time += noteDuration;
                    }

                    if (togetherRef.current != null) {
                        togetherRef.current = Math.max(togetherRef.current, totalDuration);
                    } else {
                        await new Promise((resolve) => setTimeout(resolve, totalDuration * 1000));
                    }

                    break;
                }
                default: {
                    throw new Error(`unsupported message: ${message}`);
                }
            }
        },
        cleanup: async () => {
            await waitForTogether();

            // Let the last note fade out
            await new Promise((resolve) => setTimeout(resolve, (60 / defaultTempo) * 1000));

            stopAllInstruments();

            setRunning(false);
        },
    }));

    return (
        <div className="flex">
            <Tooltip disabled={isRunning} description="Press Run to start playing music">
                <div
                    className={`flex flex-row items-center justify-center w-20 h-20 p-2 gap-2 rounded-lg overflow-hidden border-2 border-gray-100 dark:border-gray-800 transition ${
                        isRunning ? "bg-orange-50 dark:bg-orange-950" : ""
                    }`}
                >
                    <MaterialSymbol
                        icon="music_note"
                        className={`text-3xl transition ${
                            isRunning
                                ? "text-orange-500 scale-150"
                                : "text-gray-300 dark:text-gray-600 scale-100"
                        }`}
                    />
                </div>
            </Tooltip>
        </div>
    );
});

export const paletteCategories: PaletteCategory[] = [
    {
        title: "Music",
        items: [
            {
                title: 'melody : [Melody "color:#38bdf8~;;;"]',
                code: 'melody : [Melody "color:#38bdf8~;;;"]',
            },
            {
                title: 'rhythm : [Rhythm "color:#fb7185~;;;"]',
                code: 'rhythm : [Rhythm "color:#fb7185~;;;"]',
            },
            {
                title: "play",
                code: `play [Dropdown (piano , drums , ${Object.keys(soundfontInstruments).join(
                    " , ",
                )}) piano] melody`,
            },
        ],
    },
    {
        title: "Sequencing",
        items: [
            {
                title: "tempo",
                code: `tempo [Slider 200 40 600]`,
            },
            {
                title: "together",
                code: `together {\n  _\n}`,
                replace: true,
            },
        ],
    },
    {
        title: "Control",
        items: [
            {
                title: "repeat",
                code: `repeat ([Dropdown (1 , 2 , 3 , 4 , 5 , 10 , 20 , 50 , 100) 2] times) {\n  _\n}`,
                replace: true,
            },
        ],
    },
];
