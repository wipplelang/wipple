import { DiagnosticTemplate } from ".";

export const cannotReadTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Can't read {{{ value }}} from input",
            description:
                "You can't use `prompt` with {{{ value }}} because there's no way to convert the contents of a text field into {{{ value }}}.",
            help: undefined,
        },
    ],
};
