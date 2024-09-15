import { DiagnosticTemplate } from ".";

export const extraSymbolTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Extra `{{{ symbol }}}`",
            description: "Make sure this `{{{ symbol }}}` is in the right place, or remove it.",
            help: undefined,
        },
    ],
};
