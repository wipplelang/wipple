import { DiagnosticTemplate } from ".";

export const nestedLanguageDeclarationTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Language items must be declared at the top level",
            description:
                "Wipple already defines all the language items you need, so you don't need to define any yourself. Double-check your code and make sure it's doing what you expect.",
            help: undefined,
        },
    ],
};
