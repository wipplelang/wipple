import { Tooltip } from "../../../components";
import { useMemo } from "react";

// @ts-ignore
import { Piano, MidiNumbers } from "react-piano";

export const NoteAsset = (props: {
    note: string;
    tooltip?: boolean;
    disabled?: boolean;
    onClick?: () => void;
}) => {
    const description = useMemo(
        () => props.note.replace("#", "♯").replace("b", "♭").replace(/\d+/, ""),
        [props.note],
    );

    const piano = (
        <div className="piano-preview">
            <Piano
                noteRange={{
                    first: MidiNumbers.fromNote("C0"),
                    last: MidiNumbers.fromNote("B0"),
                }}
                activeNotes={[(MidiNumbers.fromNote(props.note) % 12) + 12]}
                width={60}
                keyWidthToHeight={0.4}
                playNote={() => {}}
                stopNote={() => {}}
            />
        </div>
    );

    return (
        <div
            className={`inline-block align-middle rounded-md border-2 border-gray-100 dark:border-gray-800 overflow-clip ${
                props.tooltip ?? true ? "hover:scale-105 transition-transform" : ""
            }`}
        >
            {props.tooltip ?? true ? (
                <Tooltip
                    disabled={props.disabled}
                    description={description}
                    onClick={props.onClick}
                >
                    {piano}
                </Tooltip>
            ) : (
                piano
            )}
        </div>
    );
};
