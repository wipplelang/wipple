import { DiagnosticTemplate } from ".";

export const multipleDefinitionsTemplate: DiagnosticTemplate = ({ name }) => ({
    title: `\`${name}\` has multiple definitions`,
    description: `You can't use \`${name}\` here because it could refer to two or more different values.`,
    help: undefined,
});
