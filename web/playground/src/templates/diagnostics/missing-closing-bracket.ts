import { DiagnosticTemplate } from ".";

export const missingClosingBracketTemplate: DiagnosticTemplate = () => ({
    title: "Missing closing `]`",
    description: "Every opening bracket needs a closing bracket. Try adding one at the end.",
    help: undefined,
});
