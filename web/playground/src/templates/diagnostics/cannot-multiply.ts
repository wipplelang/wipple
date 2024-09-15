import { DiagnosticTemplate } from ".";

export const cannotMultiplyTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Can't multiply {{{ left }}} and {{{ right }}}",
            description: "Multiplying these two items together isn't supported.",
            help: undefined,
        },
    ],
};
