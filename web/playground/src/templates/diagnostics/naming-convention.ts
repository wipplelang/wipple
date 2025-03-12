import { DiagnosticTemplate } from ".";

export const namingConventionTemplate: DiagnosticTemplate = ({ convention, code, suggestion }) => ({
    title: `The ${convention} \`${code}\` should be written \`${suggestion}\``,
    description:
        "You might be trying to use a naming style from a different language. Using Wipple's style makes your code more consistent for others to read.",
    help: undefined,
});
