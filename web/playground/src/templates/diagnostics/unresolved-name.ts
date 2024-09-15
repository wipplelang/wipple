import { DiagnosticTemplate } from ".";

export const unresolvedNameTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Couldn't find `{{{ name }}}`",
            description: "Double-check your code for spelling mistakes.",
            help: undefined,
        },
    ],
};
