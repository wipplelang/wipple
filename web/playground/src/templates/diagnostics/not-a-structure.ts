import { DiagnosticTemplate } from ".";

export const notAStructureTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "This code is supposed to be {{{ type }}}, but it's actually a structure",
            description:
                "You provided a structure, but you need to put {{{ expected }}} here instead.",
            help: undefined,
        },
    ],
};
