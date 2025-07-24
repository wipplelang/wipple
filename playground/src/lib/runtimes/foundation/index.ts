import { runtime } from "$lib/models/Runtime";

export default runtime({
    library: "foundation",
    commands: {
        "Input and Output": {
            show: { code: 'show "Wipple"' },
            prompt: { code: 'input : prompt "Question"' },
        },
        Control: {
            if: { code: "result : if condition then else" },
            repeat: {
                code: "repeat (2 times)",
                surround: { before: "repeat (2 times) {", after: "}" },
            },
        },
    },
    units: {
        times: {
            presets: [1, 2, 3, 4, 5, 10, 20, 50, 100],
        },
    },
});
