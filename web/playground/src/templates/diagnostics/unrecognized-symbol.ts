import { DiagnosticTemplate } from ".";

export const unrecognizedSymbolTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Unrecognized symbol `{{{ code }}}`",
            description: "This symbol isn't valid in Wipple code. Try removing it.",
            help: undefined,
        },
    ],
};
