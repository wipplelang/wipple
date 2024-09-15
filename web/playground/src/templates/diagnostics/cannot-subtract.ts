import { DiagnosticTemplate } from ".";

export const cannotSubtractTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Can't subtract {{{ left }}} by {{{ right }}}",
            description: "Subtracting these two items isn't supported.",
            help: undefined,
        },
    ],
};
