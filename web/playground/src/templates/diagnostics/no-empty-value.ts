import { DiagnosticTemplate } from ".";

export const noEmptyValueTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Can't make an empty value that's {{{ value }}}",
            description:
                'This code only works with lists, maybes, and other values that can be "empty".',
            help: undefined,
        },
    ],
};
