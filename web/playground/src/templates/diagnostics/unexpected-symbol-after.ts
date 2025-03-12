import { DiagnosticTemplate } from ".";

export const unexpectedSymbolAfterTemplate: DiagnosticTemplate = ({ symbol, location }) => ({
    title: `Unexpected ${symbol} after ${location}`,
    description: `Double-check your parentheses, or remove the ${symbol}.`,
    help: undefined,
});
