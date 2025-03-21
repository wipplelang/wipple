import { DiagnosticTemplate } from ".";

export const emptyParenthesesTemplate: DiagnosticTemplate = () => ({
    title: "Missing code between the parentheses",
    description:
        "Try putting something between the opening `(` and the closing `)`, or remove the parentheses.",
    help: undefined,
});
