import { DiagnosticTemplate } from ".";

export const mismatchedSymbolTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Expected a different symbol here",
            description:
                "You provided a `{{{ found }}}`, but you need to put a `{{{ expected }}}` here instead.",
            help: undefined,
        },
    ],
};
