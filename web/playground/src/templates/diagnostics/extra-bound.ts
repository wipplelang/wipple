import { DiagnosticTemplate } from ".";

export const extraBoundTemplate: DiagnosticTemplate = () => ({
    title: "Unexpected bound here",
    description: "Bounds aren't allowed on type and trait definitions.",
    help: undefined,
});
