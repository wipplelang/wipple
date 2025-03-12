import { DiagnosticTemplate } from ".";

export const unresolvedLanguageItemTemplate: DiagnosticTemplate = ({ name }) => ({
    title: "Couldn't process this code",
    description: `You've found a bug in Wipple — your code is correct, but it uses the \`${name}\` language item, which hasn't been defined. Please report feedback so this can be fixed!`,
    help: undefined,
});
