import { DiagnosticTemplate } from ".";

export const missingPatternsTemplate: DiagnosticTemplate = ({ patterns }) => ({
    title: `\`when\` won't match if it receives ${patterns}`,
    description: `If the input matches ${patterns}, \`when\` won't be able to handle it.`,
    help: undefined,
});
