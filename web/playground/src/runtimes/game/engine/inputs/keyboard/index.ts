import { GameInput } from "../../index";

const keyboardMap: Record<string, number> = {
    k: 32, // B
    l: 16, // A
    j: 128, // Y
    i: 64, // X
    q: 256, // L
    e: 512, // R
    Enter: 1024, // START
    z: 0, // (stick pressed)
    c: 0, // (stick pressed)
    w: 1, // Up
    s: 2, // Down
    a: 4, // Left
    d: 8, // Right
    ArrowUp: 1, // Up
    ArrowDown: 2, // Down
    ArrowLeft: 4, // Left
    ArrowRight: 8, // Right
    " ": 0, // (middle button)
};

const gamepadInput = (): GameInput => {
    let key: string | undefined;

    window.addEventListener("keydown", (event) => {
        key = event.key;
    });

    window.addEventListener("keyup", (event) => {
        key = undefined;
    });

    return {
        button: async () => (key ? keyboardMap[key] ?? 0 : 0),
    };
};

export default gamepadInput;
