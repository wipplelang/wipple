export interface Music {
    type: keyof typeof notes;
    color: string;
    notes: string[][];
}

export const maxNotes = 16;
export const maxOctave = 6;
export const defaultOctave = 3;
export const defaultTempo = 120;
export const notesInMeasure = 4;

const octaves = [1, 2, 3, 4, 5, 6];
const pitches = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];

export const notes = {
    melody: octaves.flatMap((octave) => pitches.map((pitch) => `${pitch}${octave}`)),
    rhythm: ["clap", "clave", "cowbell", "crash", "hihat", "kick", "ride", "snare", "tom"],
};
