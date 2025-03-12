import { DiagnosticTemplate } from ".";

export const notAWrapperTemplate: DiagnosticTemplate = () => ({
    title: "This pattern is supposed to match a wrapper type, but it actually matches a structure or enumeration type",
    description: "Double-check your parentheses and the type of the input you're matching.",
    help: undefined,
});
