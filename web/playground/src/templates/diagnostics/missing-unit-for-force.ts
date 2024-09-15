import { DiagnosticTemplate } from ".";

export const missingUnitForForceTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Missing unit for force after `{{{ code }}}`",
            description:
                "Try rewriting this code as `({{ code }} newtons)`, or double-check your parentheses.",
            help: undefined,
        },
    ],
};
