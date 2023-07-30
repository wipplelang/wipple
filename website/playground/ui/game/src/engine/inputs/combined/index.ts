import { GameInput } from "../../index";

const combinedInput = (...inputs: GameInput[]): GameInput => ({
    button: async () => {
        for (const input of inputs) {
            const button = await input.button();
            if (button) {
                return button;
            }
        }

        return 0;
    },
});

export default combinedInput;
