import { DiagnosticTemplate } from ".";

export const missingFieldTemplate: DiagnosticTemplate = () => ({
    title: "Expected a field here",
    description:
        "You're creating a structure type, which needs to be made entirely of fields using `::`.",
    help: undefined,
});
