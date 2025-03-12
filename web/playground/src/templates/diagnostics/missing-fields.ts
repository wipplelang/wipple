import { DiagnosticTemplate } from ".";

export const missingFieldsTemplate: DiagnosticTemplate = ({ fields }) => ({
    title: `Missing values for ${fields}`,
    description: "Try adding values for these fields using `:`.",
    help: undefined,
});
