import { DiagnosticTemplate } from ".";

export const cannotDescribeTemplate: DiagnosticTemplate = ({ value }) => ({
    title: `Can't describe ${value}`,
    description: `You can't display ${value} on the screen or use it in placeholders because it can't be converted into text.`,
    help: undefined,
});
