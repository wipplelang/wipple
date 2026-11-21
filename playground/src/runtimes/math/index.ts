import { runtime } from "@/models/Runtime";
import MathOutput from "./MathOutput.svelte";
import { sharedUnits } from "../shared";

const functionCode = `y : x -> 2 * x`;

export default runtime({
    library: "math",
    commands: {
        Functions: {
            [functionCode]: {
                documentationName: null,
                code: functionCode,
            },
        },
        Graphing: {
            plot: { code: "plot y" },
        },
        Appearance: {
            color: { code: `color (color-widget '{"color": "blue"}')` },
            "min-x": { code: "min-x -10" },
            "min-y": { code: "min-y -10" },
            "max-x": { code: "max-x 10" },
            "max-y": { code: "max-y 10" },
            resolution: { code: "resolution 0.25" },
        },
    },
    units: {
        ...sharedUnits,
        degrees: {
            presets: [10, 20, 30, 45, 60, 90, 180, 270, 360],
        },
    },
    printEnabled: true,
    Output: MathOutput,
});
