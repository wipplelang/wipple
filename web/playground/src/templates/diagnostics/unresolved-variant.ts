import { DiagnosticTemplate } from ".";

export const unresolvedVariantTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Couldn't find a variant named `{{{ name }}}`",
            description: "Double-check your code for spelling mistakes.",
            help: undefined,
        },
    ],
};
