import { DiagnosticTemplate } from ".";

export const missingUnitForDistanceTemplate: DiagnosticTemplate = ({ code }) => ({
    title: `Missing unit for distance after \`${code}\``,
    description: `Try rewriting this code as \`(${code} meters)\`, or double-check your parentheses.`,
    help: undefined,
});
