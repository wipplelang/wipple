import { DiagnosticTemplate } from ".";

export const missingRepeatTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Missing `repeat` before `{{{ code }}}`",
            description:
                "Try rewriting this code as `repeat {{ code }}`, or double-check your parentheses.",
            help: undefined,
        },
    ],
};
