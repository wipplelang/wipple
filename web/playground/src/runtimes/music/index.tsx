import type { RuntimeComponent } from "..";
import { forwardRef, useCallback, useImperativeHandle, useRef, useState } from "react";
import { PaletteCategory } from "../../models";
import { CacheStorage, Soundfont, SplendidGrandPiano } from "smplr";
import { decodeMelody } from "../../edit/melody-picker";
import { flushSync } from "react-dom";
import { MaterialSymbol } from "react-material-symbols";
import { Tooltip } from "../../components";

export interface Settings {}

const cache = new CacheStorage();

type Instrument = SplendidGrandPiano | Soundfont;

export const getAudioContext = async () => {
    const audioContext = new AudioContext();
    await audioContext.resume();
    return audioContext;
};

export const getPiano = (audioContext: AudioContext) =>
    new SplendidGrandPiano(audioContext, { storage: cache }).load;

export const soundfontInstrumets = {
    accordion: "accordion",
    saxophone: "alto_sax",
    bagpipe: "bagpipe",
    banjo: "banjo",
    brass: "brass_section",
    cello: "cello",
    clarinet: "clarinet",
    "electric-guitar": "electric_guitar_clean",
    flute: "flute",
    harmonica: "harmonica",
    harp: "orchestral_harp",
    marimba: "marimba",
    "music-box": "music_box",
    "orchestra-hit": "orchestra_hit",
    organ: "rock_organ",
    "steel-drums": "steel_drums",
    strings: "string_ensemble_1",
    timpani: "timpani",
    trombone: "trombone",
    trumpet: "trumpet",
    tuba: "tuba",
    vibraphone: "vibraphone",
    viola: "viola",
    violin: "violin",
    woodblock: "woodblock",
    xylophone: "xylophone",
};

export const getSoundfontInstrument = (instrument: string, audioContext: AudioContext) =>
    new Soundfont(audioContext, { instrument, storage: cache }).load;

export const defaultTempo = 120;

export const Music: RuntimeComponent<Settings> = forwardRef((props, ref) => {
    const [isRunning, setRunning] = useState(false);

    const audioContextRef = useRef<AudioContext>();
    const instrumentsRef = useRef<Record<string, Instrument>>({});
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
            const audioContext = await getAudioContext();
            audioContextRef.current = audioContext;

            await Promise.all([
                (async () => {
                    const piano = await getPiano(audioContext);
                    instrumentsRef.current.piano = piano;
                })(),
                ...Object.values(soundfontInstrumets).map(async (instrument) => {
                    instrumentsRef.current[instrument] = await getSoundfontInstrument(
                        instrument,
                        audioContext,
                    );
                }),
            ]);

            togetherRef.current = undefined;

            tempoRef.current = defaultTempo;

            setRunning(true);
        },
        onMessage: async (message, value) => {
            const audioContext = audioContextRef.current;
            if (!audioContext) {
                throw new Error("audio context is not initialized");
            }

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
                    const [instrumentName, encodedMelody] = value;

                    const instrument = instrumentsRef.current[instrumentName];
                    if (!instrument) {
                        throw new Error(`unknown instrument: ${instrumentName}`);
                    }

                    const melody = decodeMelody(encodedMelody);

                    const noteDuration = 60 / tempoRef.current;
                    const totalDuration = noteDuration * melody.notes.length;

                    let time = audioContext.currentTime;
                    for (const notes of melody.notes) {
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

            for (const instrument of Object.values(instrumentsRef.current)) {
                instrument.stop();
            }

            try {
                await audioContextRef.current?.close();
            } catch {
                // Already closed
            }

            instrumentsRef.current = {};

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
                title: "play",
                code: `play [Dropdown (piano , ${Object.keys(soundfontInstrumets).join(
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
