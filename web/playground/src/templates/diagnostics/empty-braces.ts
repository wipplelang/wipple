import { DiagnosticTemplate } from ".";

export const emptyBracesTemplate: DiagnosticTemplate = () => ({
    title: "Missing code between the braces",
    description:
        "Try putting `None` between the opening `{` and the closing `}` to indicate that this block does nothing.",
    help: undefined,
});
