import { DiagnosticTemplate } from ".";

export const extraInputTemplate: DiagnosticTemplate = ({ function: func, code }) => ({
    title: `Extra input to \`${func}\``,
    description: `The \`${func}\` function doesn't need that many inputs. Try removing \`${code}\` or moving it to a new line.`,
    help: undefined,
});
