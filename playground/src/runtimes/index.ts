import turtle from "./turtle";
import music from "./music";
import math from "./math";
import foundation from "./foundation";

const runtimes = {
    turtle,
    music,
    math,
    foundation,
};

export type RuntimeId = keyof typeof runtimes;

export default runtimes;
