import { DiagnosticTemplate } from ".";

export const unusedColorTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "This color isn't used anywhere",
            description: "Try writing `color` in front of it to set the pen color.",
            help: undefined,
        },
    ],
};
