import { DiagnosticTemplate } from ".";

export const missingConstantValueTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Missing value for `{{{ code }}}`",
            description:
                "You created a constant using `::`, but it's missing a value. Try adding a value on the next line using the `:`, or double-check that you intended to use `::` here.",
            help: undefined,
        },
    ],
};
