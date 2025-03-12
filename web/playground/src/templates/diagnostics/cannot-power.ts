import { DiagnosticTemplate } from ".";

export const cannotPowerTemplate: DiagnosticTemplate = ({ left, right }) => ({
    title: `Can't raise ${left} to the power of ${right}`,
    description: "Exponentiating these two items isn't supported.",
    help: undefined,
});
