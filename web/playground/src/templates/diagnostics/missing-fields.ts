import { DiagnosticTemplate } from ".";

export const missingFieldsTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Missing values for {{{ fields }}}",
            description: "Try adding values for these fields using `:`.",
            help: undefined,
        },
    ],
};
