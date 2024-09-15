import { DiagnosticTemplate } from ".";

export const missingPixelsTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Missing `pixels` after `{{{ code }}}`",
            description:
                "Try rewriting this code as `({{ code }} pixels)`, or double-check your parentheses.",
            help: undefined,
        },
    ],
};
