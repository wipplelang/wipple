import { soundfontInstrumentNames } from "$lib/assets/instruments";
import { runtime } from "$lib/models/Runtime";
import MusicOutput from "./MusicOutput.svelte";

const melody = `melody : (Music '{"music":{"type":"melody","color":"#38bdf8","notes":[[],[],[],[]]}}')`;
const rhythm = `rhythm : (Music '{"music":{"type":"rhythm","color":"#fb7185","notes":[[],[],[],[]]}}')`;

export default runtime({
    library: "music",
    commands: {
        Music: {
            [melody]: {
                documentationName: null,
                code: melody,
            },
            [rhythm]: {
                documentationName: null,
                code: rhythm,
            },
            play: {
                code: `play (Dropdown '{"selection":"piano","options":${JSON.stringify([...Object.keys(soundfontInstrumentNames), "drums"])}}') melody`,
            },
        },
        Sequencing: {
            tempo: {
                code: "tempo 120",
            },
            together: {
                code: "together",
                surround: {
                    before: "together {",
                    after: "}",
                },
            },
        },
        Control: {
            repeat: {
                code: "repeat (2 times)",
                surround: { before: "repeat (2 times) {", after: "}" },
            },
        },
    },
    units: {
        times: {
            presets: [1, 2, 3, 4, 5, 10, 20, 50, 100],
        },
    },
    Output: MusicOutput,
});
