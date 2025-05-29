import type { RuntimeComponent } from "..";
import { forwardRef, useCallback, useImperativeHandle, useRef, useState } from "react";
import { PaletteCategory } from "../../models";
import { decodeMelody, decodeRhythm } from "../../pages/edit/assets/melody-picker";
import { MaterialSymbol } from "react-material-symbols";
import { Tooltip } from "../../components";
import { Mutex } from "async-mutex";
import * as Tone from "tone";
import * as Tonal from "tonal";

export interface Instrument {
    play: (options: { note: string; time?: number; duration?: number }) => void;
    stopAll: () => void;
}

export const defaultNoteDuration = 1;

const drumMachineBaseUrl = "https://smpldsnds.github.io/drum-machines/Casio-RZ1/";

const drumMachineSampleNames: Record<string, string> = {
    clap: "clap.m4a",
    clave: "clave.m4a",
    cowbell: "cowbell.m4a",
    crash: "crash.m4a",
    hihat: "hihat-closed.m4a",
    kick: "kick.m4a",
    ride: "ride.m4a",
    snare: "snare.m4a",
    tom: "tom-1.m4a",
};

const drumMachineMutex = new Mutex();
let drumMachine: Instrument | undefined;
export const getDrumMachine = () =>
    drumMachineMutex.runExclusive(async (): Promise<Instrument> => {
        if (drumMachine) {
            return drumMachine;
        }

        const players = new Tone.Players(drumMachineSampleNames, {
            baseUrl: drumMachineBaseUrl,
            volume: -15,
        }).toDestination();

        await Tone.loaded();

        const instrument: Instrument = {
            play: ({ note, time }) => {
                if (!note) return;

                players.player(note).start(time, 0, time ?? Tone.getContext().currentTime);
            },
            stopAll: () => players.stopAll(),
        };

        drumMachine = instrument;

        return instrument;
    });

const soundfontBaseUrl = "https://gleitz.github.io/midi-js-soundfonts/FluidR3_GM/";

const soundfontInstrumentNames: Record<string, string> = {
    piano: "acoustic_grand_piano-mp3/",
    "electric-guitar": "electric_guitar_clean-mp3/",
    flute: "flute-mp3/",
    harmonica: "harmonica-mp3/",
    marimba: "marimba-mp3/",
    "orchestra-hit": "orchestra_hit-mp3/",
    strings: "string_ensemble_1-mp3/",
    trumpet: "trumpet-mp3/",
    tuba: "tuba-mp3/",
    violin: "violin-mp3/",
};

// Load one octave; other notes will be pitch-shifted from these
const noteSamples = Tonal.Range.chromatic(["C4", "B4"]);

const soundfontInstruments = Object.fromEntries(
    Object.keys(soundfontInstrumentNames).map((name) => [
        name,
        {
            mutex: new Mutex(),
            instrument: undefined as Instrument | undefined,
        },
    ]),
);

export const getDefaultSoundfontInstrument = () => getSoundfontInstrument("piano");

export const getSoundfontInstrument = async (name: string) =>
    await soundfontInstruments[name]?.mutex.runExclusive(async (): Promise<Instrument> => {
        if (soundfontInstruments[name].instrument) {
            return soundfontInstruments[name].instrument;
        }

        const sampler = new Tone.Sampler({
            urls: Object.fromEntries(noteSamples.map((note) => [note, `${note}.mp3`])),
            baseUrl: soundfontBaseUrl + soundfontInstrumentNames[name],
        }).toDestination();

        await Tone.loaded();

        const instrument: Instrument = {
            play: ({ note, time, duration }) => {
                if (!note) return;

                sampler.triggerAttackRelease(
                    Tonal.Note.fromMidi(Tonal.Midi.toMidi(note)!), // normalize
                    (duration ?? defaultNoteDuration) * 1.5,
                    time ?? Tone.getContext().currentTime,
                );
            },
            stopAll: () => {},
        };

        soundfontInstruments[name].instrument = instrument;

        return instrument;
    });

export const stopAllInstruments = () => {
    for (const instrument of Object.values(soundfontInstruments)) {
        instrument.instrument?.stopAll();
    }

    drumMachine?.stopAll();
};

export const defaultTempo = 120;

export const Music: RuntimeComponent = forwardRef((props, ref) => {
    const [isRunning, setRunning] = useState(false);

    const playedNoteRef = useRef(false);
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
        resume: () => {
            Tone.start();
        },
        initialize: async () => {
            await Promise.all(Object.values(soundfontInstrumentNames).map(getSoundfontInstrument));

            stopAllInstruments();

            playedNoteRef.current = false;
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

                    let instrument: Instrument | undefined;
                    let sequence: string[][];
                    switch (kind) {
                        case "melody": {
                            if (instrumentName === "drums") {
                                throw new Error("expected instrument, not drums");
                            }

                            instrument = await getSoundfontInstrument(instrumentName);

                            if (!instrument) {
                                throw new Error(`unknown instrument: ${instrumentName}`);
                            }

                            sequence = decodeMelody(encoded).notes;

                            break;
                        }
                        case "rhythm": {
                            if (instrumentName !== "drums") {
                                throw new Error("expected drums, not instrument");
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

                    let time = Tone.getContext().currentTime;
                    for (const notes of sequence) {
                        for (const note of notes) {
                            instrument.play({ note, time, duration: noteDuration });
                        }

                        time += noteDuration;
                    }

                    if (togetherRef.current != null) {
                        togetherRef.current = Math.max(togetherRef.current, totalDuration);
                    } else {
                        await new Promise((resolve) => setTimeout(resolve, totalDuration * 1000));
                    }

                    playedNoteRef.current = true;

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
        <div className="flex flex-1 h-full">
            <Tooltip
                disabled={isRunning}
                description={"Press Run to start playing music"}
                className="flex-1 h-full"
            >
                <div
                    className={`flex flex-row items-center justify-center flex-1 aspect-square p-2 gap-2 rounded-lg overflow-hidden border border-gray-100 dark:border-gray-800 transition ${
                        isRunning ? "bg-orange-50 dark:bg-orange-950" : ""
                    }`}
                >
                    <MaterialSymbol
                        icon="music_note"
                        className={`text-8xl transition ${
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
