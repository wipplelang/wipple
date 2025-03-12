import { DiagnosticTemplate } from ".";

export const unresolvedTraitTemplate: DiagnosticTemplate = ({ name }) => ({
    title: `Couldn't find a trait named \`${name}\``,
    description: "Double-check your code for spelling mistakes.",
    help: undefined,
});
