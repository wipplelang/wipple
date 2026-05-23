import { preset } from "./models/Preset";
import turtlePresetIcon from "./assets/turtle-preset-icon.png";
import musicPresetIcon from "./assets/music-preset-icon.png";
import mathPresetIcon from "./assets/math-preset-icon.png";
import typeCircuitsPresetIcon from "./assets/type-circuits-icon.png";

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
    preset({
        icon: mathPresetIcon,
        name: "Math",
        description: "Graph functions",
        runtime: "math",
    }),
    preset({
        icon: typeCircuitsPresetIcon,
        name: "Visualizer",
        description: "Visualize your code with Type Circuits",
        runtime: "foundation",
    }),
];

export default presets;
