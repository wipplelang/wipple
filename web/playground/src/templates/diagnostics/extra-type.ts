import { DiagnosticTemplate } from ".";

export const extraTypeTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Extra type parameter here",
            description:
                "This type doesn't need that many inputs. Try removing `{{{ code }}}` or moving it to a new line.",
            help: undefined,
        },
    ],
};
