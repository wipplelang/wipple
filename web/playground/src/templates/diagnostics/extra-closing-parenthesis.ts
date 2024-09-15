import { DiagnosticTemplate } from ".";

export const extraClosingParenthesisTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Extra closing `)`",
            description:
                "Make sure you have an opening `(` in the right place, or remove this one.",
            help: undefined,
        },
    ],
};
