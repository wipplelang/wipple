import { DiagnosticTemplate } from ".";

export const unknownTypeTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Could not determine the meaning of this code",
            description:
                "Wipple needs more information before it can run this code. Try assigning `{{{ code }}}` to a variable using `:`, and then use it somewhere else in the program, to help Wipple determine its meaning.",
            help: undefined,
        },
    ],
};
