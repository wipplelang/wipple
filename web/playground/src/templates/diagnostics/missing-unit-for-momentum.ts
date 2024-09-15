import { DiagnosticTemplate } from ".";

export const missingUnitForMomentumTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Missing unit for momentum after `{{{ code }}}`",
            description:
                "Try rewriting this code as `({{ code }} (kilograms * (meters / seconds)))`, or double-check your parentheses.",
            help: undefined,
        },
    ],
};
