import { GameInput } from "../../index";

const gamepadMap = [
    32, // B
    16, // A
    128, // Y
    64, // X
    256, // L
    512, // R
    256, // L
    512, // R
    1024, // START
    1024, // START
    0, // (stick pressed)
    0, // (stick pressed)
    1, // Up
    2, // Down
    4, // Left
    8, // Right
    0, // (middle button)
];

const gamepadInput = (): GameInput => ({
    button: async () => {
        for (const gamepad of navigator.getGamepads()) {
            if (!gamepad || gamepad.mapping !== "standard") {
                continue;
            }

            for (let button = 0; button < gamepad.buttons.length; button++) {
                if (gamepad.buttons[button].pressed) {
                    return gamepadMap[button] ?? 0;
                }
            }
        }

        return 0;
    },
});

export default gamepadInput;
