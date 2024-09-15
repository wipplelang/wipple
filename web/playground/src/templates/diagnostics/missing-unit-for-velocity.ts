import { DiagnosticTemplate } from ".";

export const missingUnitForVelocityTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Missing unit for velocity after `{{{ code }}}`",
            description:
                "Try rewriting this code as `({{ code }} (meters / seconds)`, or double-check your parentheses.",
            help: undefined,
        },
    ],
};
