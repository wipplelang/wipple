import { DiagnosticTemplate } from ".";

export const unexpectedSymbolBeforeTemplate: DiagnosticTemplate = ({ symbol, location }) => ({
    title: `Unexpected ${symbol} before ${location}`,
    description: `Double-check your parentheses, or remove the ${symbol}.`,
    help: undefined,
});
