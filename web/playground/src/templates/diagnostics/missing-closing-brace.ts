import { DiagnosticTemplate } from ".";

export const missingClosingBraceTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Missing closing `}`",
            description: "Every opening brace needs a closing brace. Try adding one at the end.",
            help: undefined,
        },
    ],
};
