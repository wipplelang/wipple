import { DiagnosticTemplate } from ".";

export const partiallyUnknownTypeTemplate: DiagnosticTemplate = ({ type }) => ({
    title: "Could not determine the meaning of this code",
    description: `Wipple needs more information before it can run this code. Its type is ${type}, but the \`_\` placeholders are unknown.`,
    help: undefined,
});
