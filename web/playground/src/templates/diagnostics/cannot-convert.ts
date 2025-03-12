import { DiagnosticTemplate } from ".";

export const cannotConvertTemplate: DiagnosticTemplate = ({ input, output }) => ({
    title: `Can't convert ${input} to ${output}`,
    description: `Converting this value into ${output} isn't supported.`,
    help: undefined,
});
