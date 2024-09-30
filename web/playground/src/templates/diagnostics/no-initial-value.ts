import { DiagnosticTemplate } from ".";

export const noInitialValueTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Can't create an initial {{{ value }}} value",
            description:
                "This code only works with lists, maybes, and other types that have an initial value.",
            help: undefined,
        },
    ],
};
