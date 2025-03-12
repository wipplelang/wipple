import { DiagnosticTemplate } from ".";

export const traitHasNoValueTemplate: DiagnosticTemplate = ({ trait }) => ({
    title: `Can't use \`${trait}\` as a value`,
    description: `\`${trait}\` can only be used in \`where\` bounds, not as a value.`,
    help: undefined,
});
