import { DiagnosticTemplate } from ".";

export const missingUnitForMassTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Missing unit for mass after `{{{ code }}}`",
            description:
                "Try rewriting this code as `({{ code }} kilograms)`, or double-check your parentheses.",
            help: undefined,
        },
    ],
};