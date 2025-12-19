const __wipple_env = {
    display: console.log,
    prompt: async (message, submit) => {
        const rl = require("node:readline/promises").createInterface({
            input: process.stdin,
            output: process.stdout,
        });

        let valid = false;
        while (!valid) {
            const input = await rl.question(message + ": ");
            valid = await submit(input);
        }

        rl.close();
    },
};
