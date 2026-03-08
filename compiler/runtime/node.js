let rl;

const __wipple_env = {
    display: (message) => console.log(message),
    prompt: async (message) => {
        rl = require("node:readline/promises").createInterface({
            input: process.stdin,
            output: process.stdout,
        });

        return await rl.question(message + ": ");
    },
    validatePrompt: async (valid) => {
        if (valid) {
            rl.close();
            rl = undefined;
        } else {
            return await rl.question("try again: ");
        }
    },
};
