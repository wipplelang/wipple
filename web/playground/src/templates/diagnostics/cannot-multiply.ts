import { DiagnosticTemplate } from ".";

export const cannotMultiplyTemplate: DiagnosticTemplate = ({ left, right }) => ({
    title: `Can't multiply ${left} and ${right}`,
    description: "Multiplying these two items together isn't supported.",
    help: undefined,
});
