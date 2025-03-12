import { DiagnosticTemplate } from ".";

export const invalidPlaceholderTextTemplate: DiagnosticTemplate = ({ expected, found }) => ({
    title: `This text needs ${expected} inputs, but you provided ${found}`,
    description:
        "You need to provide the same number of items as there are `_` placeholders inside the text. Make sure you're putting your parentheses in the right places.",
    help: undefined,
});
