import { DiagnosticTemplate } from ".";

export const missingUnitForAngleTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Missing `degrees` after `{{{ code }}}`",
            description:
                "Try rewriting this code as `({{ code }} degrees)`, or double-check your parentheses.",
            help: undefined,
        },
    ],
};
