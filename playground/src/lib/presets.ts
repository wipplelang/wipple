import { preset } from "./models/Preset";
import blankPresetIcon from "./assets/blank-preset-icon.png";
// import mathPresetIcon from "./assets/math-preset-icon.png";
import musicPresetIcon from "./assets/music-preset-icon.png";
import turtlePresetIcon from "./assets/turtle-preset-icon.png";

const presets = [
    preset({
        name: "Turtle",
        icon: turtlePresetIcon,
        backgroundClassName: "bg-green-100",
        runtime: "turtle",
    }),
    preset({
        name: "Music",
        icon: musicPresetIcon,
        backgroundClassName: "bg-orange-100",
        runtime: "music",
    }),
    // preset({
    //     name: "Math",
    //     icon: mathPresetIcon,
    //     backgroundClassName: "bg-blue-100",
    //     runtime: "math",
    // }),
    preset({
        name: "Blank",
        icon: blankPresetIcon,
        backgroundClassName: "bg-slate-100",
        runtime: "foundation",
    }),
];

export default presets;
