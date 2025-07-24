import { runtime } from "$lib/models/Runtime";
import TurtleOutput from "./TurtleOutput.svelte";

export default runtime({
    library: "turtle",
    commands: {
        Movement: {
            forward: { code: "forward (50 pixels)" },
            backward: { code: "backward (50 pixels)" },
            left: { code: "left (90 degrees)" },
            right: { code: "right (90 degrees)" },
        },
        Appearance: {
            color: { code: `color (Color '{"color": "blue"}')` },
            speed: { code: "speed 0.5" },
            animal: { code: `animal (Animal '{"animal": "frog"}')` },
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
        pixels: {
            presets: [10, 20, 30, 40, 50, 100, 200, 300],
        },
        degrees: {
            presets: [10, 20, 30, 45, 60, 90, 180, 270, 360],
        },
    },
    printEnabled: true,
    Output: TurtleOutput,
});
