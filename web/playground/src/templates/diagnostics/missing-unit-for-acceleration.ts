import { DiagnosticTemplate } from ".";

export const missingUnitForAccelerationTemplate: DiagnosticTemplate = ({ code }) => ({
    title: `Missing unit for acceleration after \`${code}\``,
    description: `Try rewriting this code as \`(${code} (meters / seconds / seconds))\`, or double-check your parentheses.`,
    help: undefined,
});
