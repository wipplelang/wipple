import { DiagnosticTemplate } from ".";

export const unresolvedTypeTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Couldn't find a type named `{{{ name }}}`",
            description: "Double-check your code for spelling mistakes.",
            help: undefined,
        },
    ],
};
