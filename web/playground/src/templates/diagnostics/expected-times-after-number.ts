import { DiagnosticTemplate } from ".";

export const expectedTimesAfterNumberTemplate: DiagnosticTemplate = ({ code }) => ({
    title: `Missing \`times\` after \`${code}\``,
    description: `Try rewriting this code as \`(${code} times)\`, or double-check your parentheses.`,
    help: undefined,
});
