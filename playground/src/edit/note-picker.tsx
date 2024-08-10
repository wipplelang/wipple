import { Button } from "../components";
import { useEffect, useMemo, useRef, useState } from "react";
import { Player, instrument } from "soundfont-player";

// @ts-ignore
import { ControlledPiano, MidiNumbers } from "react-piano";

export const NotePicker = (props: { selection: string; onDismiss: (note: string) => void }) => {
    const [selection, setSelection] = useState(props.selection);

    const audioContext = useMemo(() => {
        const audioContext = new AudioContext();
        audioContext.resume();
        return audioContext;
    }, []);

    const player = useRef<Player>();
    useEffect(() => {
        (async () => {
            try {
                player.current = await instrument(audioContext, "acoustic_grand_piano");
            } catch (error) {
                console.error(error);
            }
        })();
    }, [audioContext]);

    return (
        <div className="flex flex-col gap-4 w-[512px]">
            <h1 className="text-2xl font-semibold">Choose a Note</h1>

            <ControlledPiano
                noteRange={{
                    first: MidiNumbers.fromNote("C3"),
                    last: MidiNumbers.fromNote("C7"),
                }}
                activeNotes={[MidiNumbers.fromNote(selection)]}
                width={512}
                keyWidthToHeight={0.275}
                onPlayNoteInput={(midiNumber: any) => {
                    const note = MidiNumbers.getAttributes(midiNumber).note;
                    player.current?.play(note, audioContext.currentTime, { duration: 1 });
                    setSelection(note);
                }}
                onStopNoteInput={() => {}}
                playNote={() => {}}
                stopNote={() => {}}
                renderNoteLabel={({ midiNumber, isActive, isAccidental }: any) =>
                    !isAccidental ? (
                        <p
                            className={`text-[8pt] text-center pointer-events-none select-none ${
                                isActive ? "text-white" : "text-gray-700"
                            }`}
                        >
                            {MidiNumbers.getAttributes(midiNumber).pitchName}
                        </p>
                    ) : null
                }
            />

            <Button role="primary" fill onClick={() => props.onDismiss(selection)}>
                Done
            </Button>
        </div>
    );
};
