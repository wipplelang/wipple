import { DiagnosticTemplate } from ".";

export const cannotOrderTemplate: DiagnosticTemplate = ({ value }) => ({
    title: `Can't put items that are ${value} into order`,
    description: `You can't use \`>\` or \`<\` with ${value} because it doesn't support ordering.`,
    help: undefined,
});
