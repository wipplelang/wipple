import { DiagnosticTemplate } from ".";

export const missingUnitForTimeTemplate: DiagnosticTemplate = ({ code }) => ({
    title: `Missing unit for time after \`${code}\``,
    description: `Try rewriting this code as \`(${code} seconds\`, or double-check your parentheses.`,
    help: undefined,
});
