import { DiagnosticTemplate } from ".";

export const instanceAlreadyExistsTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "`{{{ code }}}` already exists",
            description: "You can't define two instances that match the same types.",
            help: undefined,
        },
    ],
};
