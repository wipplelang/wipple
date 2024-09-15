import { DiagnosticTemplate } from ".";

export const missingPatternsTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "`{{{ code }}}` won't match if it receives {{{ patterns }}}",
            description:
                "If the input matches {{{ patterns }}}, `{{{ code }}}` won't be able to handle it.",
            help: undefined,
        },
    ],
};
