import turtle from "./turtle";
import music from "./music";
import foundation from "./foundation";

const runtimes = {
    turtle,
    music,
    foundation,
};

export type RuntimeId = keyof typeof runtimes;

export default runtimes;
