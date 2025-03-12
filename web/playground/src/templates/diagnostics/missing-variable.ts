import { DiagnosticTemplate } from ".";

export const missingVariableTemplate: DiagnosticTemplate = ({ code }) => ({
    title: "Missing a `_` pattern to match remaining inputs",
    description: `Try adding one more pattern for \`_\` to match anything not already matched in \`${code}\`.`,
    help: undefined,
});
