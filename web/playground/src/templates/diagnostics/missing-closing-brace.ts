import { DiagnosticTemplate } from ".";

export const missingClosingBraceTemplate: DiagnosticTemplate = () => ({
    title: "Missing closing `}`",
    description: "Every opening brace needs a closing brace. Try adding one at the end.",
    help: undefined,
});
