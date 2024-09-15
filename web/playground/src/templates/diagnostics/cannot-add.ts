import { DiagnosticTemplate } from ".";

export const cannotAddTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Can't add {{{ left }}} and {{{ right }}}",
            description: "Adding these two items together isn't supported.",
            help: undefined,
        },
    ],
};
