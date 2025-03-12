import { DiagnosticTemplate } from ".";

export const unresolvedNameTemplate: DiagnosticTemplate = ({ name }) => ({
    title: `Couldn't find \`${name}\``,
    description: "Double-check your code for spelling mistakes.",
    help: undefined,
});
