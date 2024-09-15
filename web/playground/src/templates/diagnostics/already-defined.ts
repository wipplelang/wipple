import { DiagnosticTemplate } from ".";

export const alreadyDefinedTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "`{}` is already defined",
            description: "This name is already chosen. You'll have to choose a different one.",
            help: undefined,
        },
    ],
};
