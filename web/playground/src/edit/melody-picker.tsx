import * as Tonal from "tonal";
import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { produce } from "immer";
import { Animated, ContextMenuButton, Tooltip } from "../components";
import { MaterialSymbol } from "react-material-symbols";
import randomColor from "randomcolor";
import { getPiano } from "../runtimes/music";
import { SplendidGrandPiano } from "smplr";

export interface Melody {
    options: {
        color: string | undefined;
        octave: number;
        key:
            | {
                  type: "major";
                  tonic: string;
              }
            | undefined;
    };
    notes: string[][];
}

export const encodeMelody = (melody: Melody) => {
    const encodedOptions: string[] = [];

    if (melody.options.color) {
        encodedOptions.push(`color:${melody.options.color}`);
    }

    encodedOptions.push(`octave:${melody.options.octave}`);

    if (melody.options.key) {
        encodedOptions.push(`key:${melody.options.key.tonic} ${melody.options.key.type}`);
    }

    return `${encodedOptions.join(";")}~${melody.notes.map((notes) => notes.join(",")).join(";")}`;
};

export const decodeMelody = (melody: string): Melody => {
    const [encodedOptions, encodedNotes] = melody.includes("~") ? melody.split("~") : ["", melody];

    const rawOptions = Object.fromEntries(
        encodedOptions.split(";").map((option) => option.split(":")) as [string, string][],
    );

    const options = {
        color: rawOptions.color ? rawOptions.color : undefined,
        octave: rawOptions.octave ? parseInt(rawOptions.octave, 10) : defaultOctave,
        key: rawOptions.key
            ? (() => {
                  const [tonic, type] = rawOptions.key.split(" ");

                  if (type === "major") {
                      return { type, tonic } as const;
                  } else {
                      return undefined;
                  }
              })()
            : undefined,
    };

    const notes = encodedNotes.split(";").map((notes) => notes.split(","));

    return { options, notes };
};

const noteNames = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];

const keys = [
    { key: Tonal.Key.majorKey("C"), major: "C Major", minor: "A Minor" },
    { key: Tonal.Key.majorKey("C#"), major: "C#/Db Major", minor: "A#/Bb Minor" },
    { key: Tonal.Key.majorKey("D"), major: "D Major", minor: "B Minor" },
    { key: Tonal.Key.majorKey("D#"), major: "D#/Eb Major", minor: "C Minor" },
    { key: Tonal.Key.majorKey("E"), major: "E Major", minor: "C#/Db Minor" },
    { key: Tonal.Key.majorKey("F"), major: "F Major", minor: "D Minor" },
    { key: Tonal.Key.majorKey("F#"), major: "F#/Gb Major", minor: "D#/Eb Minor" },
    { key: Tonal.Key.majorKey("G"), major: "G Major", minor: "E Minor" },
    { key: Tonal.Key.majorKey("G#"), major: "G#/Ab Major", minor: "F Minor" },
    { key: Tonal.Key.majorKey("A"), major: "A Major", minor: "F#/Gb Minor" },
    { key: Tonal.Key.majorKey("A#"), major: "A#/Bb Major", minor: "G Minor" },
    { key: Tonal.Key.majorKey("B"), major: "B Major", minor: "G#/Ab Minor" },
];

const maxNotes = 16;
const maxOctave = 6;
const defaultOctave = 3;

export const MelodyPicker = (props: { selection: string; onDismiss: (melody: string) => void }) => {
    const [melody, setMelody] = useState(decodeMelody(props.selection));

    const pianoRef = useRef<SplendidGrandPiano>();

    const [playingId, setPlayingId] = useState<Symbol>();

    const isPlaying = playingId != null;

    const playingIdRef = useRef<Symbol>();
    useEffect(() => {
        playingIdRef.current = playingId;
    }, [playingId]);

    const play = useCallback(async () => {
        try {
            const playingId = Symbol();
            setPlayingId(playingId);

            const piano = await getPiano();
            pianoRef.current = piano;

            const totalDuration = 2;
            const noteDuration = totalDuration / melody.notes.length;

            let time = piano.context.currentTime;
            for (const notes of melody.notes) {
                for (const note of notes) {
                    piano.start({ note, time });
                }

                time += noteDuration;
            }

            setTimeout(() => {
                if (playingIdRef.current === playingId) {
                    setPlayingId(undefined);
                }
            }, totalDuration * 1000);
        } catch {
            setPlayingId(undefined);
        }
    }, [melody]);

    const stop = useCallback(() => {
        pianoRef.current?.stop();
        setPlayingId(undefined);
    }, []);

    const sampleNote = useCallback(async (note: string) => {
        const piano = await getPiano();
        console.log("piano", piano);
        piano.start({ note });
    }, []);

    return (
        <div className="flex flex-col gap-4 w-[600px]">
            <div className="flex flex-row items-stretch justify-between">
                <div className="flex flex-row gap-2">
                    <ChangeColorButton
                        color={melody.options.color}
                        onClick={() => {
                            setMelody(
                                produce((melody) => {
                                    melody.options.color = randomColor();
                                }),
                            );
                        }}
                    />

                    <PlayButton isPlaying={isPlaying} onClick={isPlaying ? stop : play} />

                    <ClearButton
                        onClick={() => {
                            if (confirm("Are you sure?")) {
                                setMelody(
                                    produce((melody) => {
                                        melody.notes = melody.notes.map(() => []);
                                    }),
                                );
                            }
                        }}
                    />
                </div>

                <DoneButton onClick={() => props.onDismiss(encodeMelody(melody))} />
            </div>

            <div className="flex flex-row items-center gap-2">
                <Option
                    options={[
                        {
                            title: "All Keys",
                            isSelected: melody.options.key == null,
                            onClick: () => {
                                setMelody(
                                    produce((melody) => {
                                        melody.options.key = undefined;
                                    }),
                                );
                            },
                        },
                        ...keys.map((key) => ({
                            title: (
                                <span>
                                    {key.major} <span className="opacity-50">({key.minor})</span>
                                </span>
                            ),
                            isSelected:
                                melody.options.key?.type === "major" &&
                                melody.options.key.tonic === key.key.tonic,
                            onClick: () => {
                                setMelody(
                                    produce((melody) => {
                                        melody.options.key = {
                                            type: "major",
                                            tonic: key.key.tonic,
                                        };
                                    }),
                                );
                            },
                        })),
                    ]}
                >
                    {melody.options.key
                        ? melody.options.key.type === "major"
                            ? keys.find((key) => key.key.tonic === melody.options.key!.tonic)
                                  ?.major ?? `${melody.options.key.tonic} Major`
                            : `(unknown key)`
                        : "All Keys"}
                </Option>

                <OptionDivider />

                <Option
                    options={new Array(maxNotes).fill(undefined).map((_, n) => {
                        const note = n + 1;

                        return {
                            title: `${note}`,
                            isSelected: note === melody.notes.length,
                            onClick: () => {
                                setMelody(
                                    produce((melody) => {
                                        if (note > melody.notes.length) {
                                            melody.notes = [
                                                ...melody.notes,
                                                ...new Array(note - melody.notes.length).fill([]),
                                            ];
                                        } else {
                                            melody.notes = melody.notes.slice(0, note);
                                        }
                                    }),
                                );
                            },
                        };
                    })}
                >
                    {`${melody.notes.length} Notes`}
                </Option>

                <OptionDivider />

                <Option
                    options={new Array(maxOctave).fill(undefined).map((_, n) => {
                        const octave = maxOctave - n;

                        return {
                            title: `${octave}`,
                            isSelected: melody.options.octave === octave,
                            onClick: () => {
                                setMelody(
                                    produce((melody) => {
                                        melody.options.octave = octave;
                                    }),
                                );
                            },
                        };
                    })}
                >
                    Octave
                </Option>
            </div>

            <Animated direction="vertical">
                <PianoRollEditor melody={melody} onChange={setMelody} onSampleNote={sampleNote} />
            </Animated>
        </div>
    );
};

const ChangeColorButton = (props: { color: string | undefined; onClick: () => void }) => (
    <Tooltip description="Change Color" onClick={props.onClick}>
        <div className="rounded-lg border-2 border-gray-100 dark:border-gray-800 overflow-clip hover:scale-110 transition-transform">
            <div
                className="flex items-center justify-center w-9 h-9"
                style={{ backgroundColor: props.color ?? "gray" }}
            >
                <MaterialSymbol icon="music_note" className="text-white text-2xl" />
            </div>
        </div>
    </Tooltip>
);

const PlayButton = (props: { isPlaying: boolean; onClick: () => void }) => (
    <button
        onClick={props.onClick}
        className={`flex flex-row items-center justify-center gap-0.5 px-2.5 transition-colors rounded-lg ${
            props.isPlaying
                ? "bg-red-100 dark:bg-red-950 text-red-500 hover:bg-red-500 dark:hover:bg-red-500 hover:text-white"
                : "bg-blue-100 dark:bg-blue-950 text-blue-500 hover:bg-blue-500 dark:hover:bg-blue-500 hover:text-white"
        }`}
    >
        <MaterialSymbol icon={props.isPlaying ? "stop" : "play_arrow"} fill className="text-2xl" />

        {props.isPlaying ? "Stop" : "Play"}
    </button>
);

const ClearButton = (props: { onClick: () => void }) => (
    <button
        onClick={props.onClick}
        className="flex flex-row items-center justify-center gap-0.5 px-2.5 transition-colors rounded-lg bg-red-100 dark:bg-red-950 text-red-500 hover:bg-red-500 dark:hover:bg-red-500 hover:text-white"
    >
        Clear
    </button>
);

const DoneButton = (props: { onClick: () => void }) => (
    <button
        onClick={props.onClick}
        className="flex flex-row items-center justify-center gap-0.5 px-2.5 transition-colors text-white font-semibold rounded-lg bg-blue-500 hover:bg-blue-600 dark:hover:bg-blue-400"
    >
        Done
    </button>
);

const Option = (props: {
    options: { title: string | JSX.Element; isSelected: boolean; onClick: () => void }[];
    children: string;
}) => (
    <ContextMenuButton
        items={props.options.map((option) => ({
            title: () => (
                <>
                    <span className={`text-blue-500 ${option.isSelected ? "" : "opacity-0"}`}>
                        {"✓ "}
                    </span>

                    {option.title}
                </>
            ),
            onClick: option.onClick,
        }))}
        className="cursor-pointer"
    >
        <div className="pl-2 w-fit flex flex-row items-center gap-0.5 rounded-lg hover:bg-gray-50 dark:hover:bg-gray-800 transition-colors">
            <span className="text-sm">{props.children}</span>

            <MaterialSymbol icon="keyboard_arrow_down" fill className="text-xl text-blue-500" />
        </div>
    </ContextMenuButton>
);

const OptionDivider = () => (
    <div className="w-[2px] h-6 bg-gray-100 dark:bg-gray-800 rounded-full" />
);

const PianoRollEditor = (props: {
    melody: Melody;
    onChange: (melody: Melody) => void;
    onSampleNote: (note: string) => void;
}) => {
    const allNotes = useMemo(() => {
        const scaleNotes =
            props.melody.options.key?.type === "major"
                ? Tonal.Key.majorKey(props.melody.options.key.tonic)
                      .scale.map((note) => {
                          // Normalize note name
                          const midi = Tonal.Note.midi(`${note}0`)!;
                          return Tonal.Note.get(props.melody.options.key!.tonic).acc === "#"
                              ? Tonal.Note.fromMidiSharps(midi)
                              : Tonal.Note.fromMidi(midi);
                      })
                      .map((note) => Tonal.Note.get(note).pc)
                : noteNames;

        let octave = props.melody.options.octave;
        let prevChroma = Tonal.Note.get(scaleNotes[0]).chroma;
        return [scaleNotes, scaleNotes].flatMap((notes) =>
            notes.map((note) => {
                const chroma = Tonal.Note.get(note).chroma;

                // Increment octave when note wraps around
                if (chroma < prevChroma) {
                    octave += 1;
                }

                prevChroma = chroma;

                return `${note}${octave}`;
            }),
        );
    }, [props.melody.options.octave, props.melody.options.key]);

    return (
        <div className="flex flex-col-reverse border-2 border-gray-50 dark:border-gray-800 rounded-lg overflow-clip">
            {allNotes.map((note, index) => (
                <PianoRollEditorRow
                    key={index}
                    note={note}
                    items={props.melody.notes.map((notes) => notes.includes(note))}
                    onChange={(items) => {
                        props.onChange(
                            produce(props.melody, (melody) => {
                                items.forEach((selected, index) => {
                                    if (selected) {
                                        if (!melody.notes[index].includes(note)) {
                                            melody.notes[index] = sortNotes([
                                                ...melody.notes[index],
                                                note,
                                            ]);
                                        }
                                    } else {
                                        melody.notes[index] = sortNotes(
                                            melody.notes[index].filter((n) => n !== note),
                                        );
                                    }
                                });
                            }),
                        );
                    }}
                    onSampleNote={() => props.onSampleNote(note)}
                />
            ))}
        </div>
    );
};

const PianoRollEditorRow = (props: {
    note: string;
    items: boolean[];
    onChange: (items: boolean[]) => void;
    onSampleNote: () => void;
}) => {
    const note = useMemo(() => Tonal.Note.get(props.note), [props.note]);

    const name = useMemo(() => {
        if (note.acc === "") {
            return note.pc;
        }

        const sharp = Tonal.Note.get(Tonal.Note.fromMidiSharps(Tonal.Note.midi(`${note.pc}0`)!)).pc;
        const flat = Tonal.Note.get(Tonal.Note.fromMidi(Tonal.Note.midi(`${note.pc}0`)!)).pc;

        return `${sharp}/${flat}`;
    }, [note]);

    return (
        <div className="flex flex-row h-5">
            <button
                className={`flex flex-row items-center justify-end w-20 outline outline-[1px] outline-gray-50 dark:outline-gray-800 px-1 z-10 ${
                    note.acc ? "bg-black hover:bg-gray-700" : "bg-white hover:bg-gray-200"
                }`}
                onClick={props.onSampleNote}
            >
                <div className="text-xs text-gray-400">{name}</div>
            </button>

            {props.items.map((selected, index) => (
                <div key={index} className="flex-1 relative">
                    <button
                        key={index}
                        className={`absolute inset-0 border border-gray-50 dark:border-gray-800 ${
                            selected
                                ? "bg-blue-500 hover:bg-blue-400 dark:hover:bg-blue-600"
                                : "hover:bg-blue-50 dark:hover:bg-blue-950"
                        }`}
                        onClick={() => {
                            if (!props.items[index]) {
                                props.onSampleNote();
                            }

                            props.onChange(
                                produce(props.items, (items) => {
                                    items[index] = !items[index];
                                }),
                            );
                        }}
                    />

                    <div className="absolute inset-0 z-10 w-0.5 bg-gray-200 dark:bg-gray-700" />
                </div>
            ))}
        </div>
    );
};

const sortNotes = (notes: string[]) =>
    notes.sort((a, b) => (Tonal.Note.get(a).midi ?? 0) - (Tonal.Note.get(b).midi ?? 0));
