import { DiagnosticTemplate } from ".";

export const unresolvedTraitTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Couldn't find a trait named `{{{ name }}}`",
            description: "Double-check your code for spelling mistakes.",
            help: undefined,
        },
    ],
};
