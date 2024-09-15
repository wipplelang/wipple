import { DiagnosticTemplate } from ".";

export const missingInstanceValueTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Missing a value for this instance",
            description: "Try adding a value using `:`.",
            help: undefined,
        },
    ],
};
