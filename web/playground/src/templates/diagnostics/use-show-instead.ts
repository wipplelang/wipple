import { DiagnosticTemplate } from ".";

export const useShowInsteadTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Output is written using `show` in Wipple, not `{{{ code }}}`",
            description: "Try changing your code to use `show`.",
            help: undefined,
        },
    ],
};
