import { DiagnosticTemplate } from ".";

export const unrecognizedSymbolTemplate: DiagnosticTemplate = ({ code }) => ({
    title: `Unrecognized symbol \`${code}\``,
    description: "This symbol isn't valid in Wipple code. Try removing it.",
    help: undefined,
});
