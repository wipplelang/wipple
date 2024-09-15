import { DiagnosticTemplate } from ".";

export const unexpectedInstanceValueTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "This instance doesn't need a value",
            description:
                "You provided a value here using `:`, but the corresponding trait doesn't need a value. Try removing the value and the `:`, leaving just `{{{ code }}}`.",
            help: undefined,
        },
    ],
};
