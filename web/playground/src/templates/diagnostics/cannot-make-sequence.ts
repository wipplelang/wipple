import { DiagnosticTemplate } from ".";

export const cannotMakeSequenceTemplate: DiagnosticTemplate = ({ container }) => ({
    title: `Can't make a sequence from ${container}`,
    description: `You can't use ${container} as a sequence. Try using a list instead.`,
    help: undefined,
});
