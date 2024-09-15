import { DiagnosticTemplate } from ".";

export const extraInputTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Extra input to `{{ function }} `",
            description:
                "The `{{{ function }}}` function doesn't need that many inputs. Try removing `{{{ code }}}` or moving it to a new line.",
            help: undefined,
        },
    ],
};
