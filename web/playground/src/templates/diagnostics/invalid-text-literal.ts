import { DiagnosticTemplate } from ".";

export const invalidTextLiteralTemplate: DiagnosticTemplate = ({ message }) => ({
    title: `Invalid text: ${message}`,
    description:
        "Some characters aren't allowed inside text, or you might be using an invalid escape sequence.",
    help: undefined,
});
