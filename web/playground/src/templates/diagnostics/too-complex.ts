import { DiagnosticTemplate } from ".";

export const tooComplexTemplate: DiagnosticTemplate = () => ({
    title: "This code is too complex to check",
    description:
        "Wipple ran out of time while checking this code. Try breaking it into smaller pieces or adding more type annotations using `::`.",
    help: undefined,
});
