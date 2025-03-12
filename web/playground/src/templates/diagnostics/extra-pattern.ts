import { DiagnosticTemplate } from ".";

export const extraPatternTemplate: DiagnosticTemplate = () => ({
    title: "Extra pattern",
    description:
        "This pattern will never be matched because another pattern above it matches the input already.",
    help: undefined,
});
