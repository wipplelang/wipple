import { DiagnosticTemplate } from ".";

export const missingSymbolTemplate: DiagnosticTemplate = ({ symbol }) => ({
    title: `Missing \`${symbol}\` here`,
    description: "Try adding this symbol, or double-check your parentheses.",
    help: undefined,
});
