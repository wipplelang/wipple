import { DiagnosticTemplate } from ".";

export const cannotDivideTemplate: DiagnosticTemplate = ({ left, right }) => ({
    title: `Can't divide ${left} by ${right}`,
    description: "Dividing these two items isn't supported.",
    help: undefined,
});
