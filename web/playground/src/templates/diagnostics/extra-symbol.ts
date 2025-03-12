import { DiagnosticTemplate } from ".";

export const extraSymbolTemplate: DiagnosticTemplate = ({ symbol }) => ({
    title: `Extra \`${symbol}\``,
    description: `Make sure this \`${symbol}\` is in the right place, or remove it.`,
    help: undefined,
});
