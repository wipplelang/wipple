import { preset } from "./models/Preset";
import turtlePresetIcon from "./assets/turtle-preset-icon.png";
import musicPresetIcon from "./assets/music-preset-icon.png";
// import mathPresetIcon from "./assets/math-preset-icon.png";
import blankPresetIcon from "./assets/blank-preset-icon.png";

const presets = [
    preset({
        icon: turtlePresetIcon,
        name: "Turtle",
        description: "Create drawings",
        runtime: "turtle",
    }),
    preset({
        icon: musicPresetIcon,
        name: "Music",
        description: "Make a composition",
        runtime: "music",
    }),
    // preset({
    //     icon: mathPresetIcon,
    //     name: "Math",
    //     description: "Graph functions",
    //     runtime: "math",
    // }),
    preset({
        icon: blankPresetIcon,
        name: "No template",
        description: "A blank canvas",
        runtime: "foundation",
    }),
];

export default presets;
