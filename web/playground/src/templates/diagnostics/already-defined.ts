import { DiagnosticTemplate } from ".";

export const alreadyDefinedTemplate: DiagnosticTemplate = ({ name }) => ({
    title: `\`${name}\` is already defined`,
    description: "This name is already chosen. You'll have to choose a different one.",
    help: undefined,
});
