import { DiagnosticTemplate } from ".";

export const cannotRemainderTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Can't divide {{{ left }}} by {{{ right }}} and get the remainder",
            description: "Dividing these two items with remainder isn't supported.",
            help: undefined,
        },
    ],
};