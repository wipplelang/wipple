import * as tonal from "tonal";
import "./Opus Note Names.otf";

export const Note = (props: { children: string }) => {
    const note = tonal.Note.get(props.children)!;
    const name = `${note.letter}${note.acc}`;

    const character = {
        Ab: 0x62,
        A: 0x63,
        "A#": 0x64,
        Bb: 0x65,
        B: 0x66,
        C: 0x69,
        "C#": 0x6a,
        Db: 0x6b,
        D: 0x6c,
        "D#": 0x6d,
        Eb: 0x6e,
        E: 0x6f,
        F: 0x72,
        "F#": 0x73,
        Gb: 0x74,
        G: 0x75,
        "G#": 0x76,
    }[name];

    if (!character) return null;

    return (
        <p
            style={{
                position: "absolute",
                top: "-0.67em",
                left: "10%",
                fontSize: "16pt",
                fontFamily: "Opus Note Names",
            }}
        >
            {String.fromCharCode(character)}
        </p>
    );
};
