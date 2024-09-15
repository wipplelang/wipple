import { DiagnosticTemplate } from ".";

export const missingVariantTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Expected a variant here",
            description:
                "You're creating an enumeration type, which needs to be made entirely of variants. You can't mix in fields with `::`.",
            help: undefined,
        },
    ],
};
