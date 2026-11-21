import { runtime } from "@/models/Runtime";
import { sharedUnits } from "../shared";

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
        ...sharedUnits,
    },
    hoverEnabled: true,
});
