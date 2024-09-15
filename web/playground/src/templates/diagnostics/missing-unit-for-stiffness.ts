import { DiagnosticTemplate } from ".";

export const missingUnitForStiffnessTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Missing unit for stiffness after `{{{ code }}}`",
            description:
                "Try rewriting this code as `({{ code }} (newtons / meters)`, or double-check your parentheses.",
            help: undefined,
        },
    ],
};
