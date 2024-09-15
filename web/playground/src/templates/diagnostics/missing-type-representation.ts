import { DiagnosticTemplate } from ".";

export const missingTypeRepresentationTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Missing fields or variants between the `{ }` for this type",
            description: "If you're trying to create a type without any fields, remove the braces.",
            help: undefined,
        },
    ],
};
