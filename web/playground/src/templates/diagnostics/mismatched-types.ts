import { DiagnosticTemplate } from ".";

export const mismatchedTypesTemplate: DiagnosticTemplate = ({ found, expected }) => ({
    title: `This code is supposed to be ${found}, but it's actually ${expected}`,
    description: `You provided ${found}, but you need to put ${expected} here instead.`,
    help: undefined,
});
