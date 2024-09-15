import { DiagnosticTemplate } from ".";

export const missingTypesTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Missing {{ count }} type parameters here",
            description:
                "You might be missing parentheses to group `{{{ code }}}` with any types after it.",
            help: undefined,
        },
    ],
};
