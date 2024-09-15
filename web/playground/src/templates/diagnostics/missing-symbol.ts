import { DiagnosticTemplate } from ".";

export const missingSymbolTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Missing `{{{ symbol }}}` here",
            description: "Try adding this symbol, or double-check your parentheses.",
            help: undefined,
        },
    ],
};
