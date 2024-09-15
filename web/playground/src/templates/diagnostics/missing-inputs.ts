import { DiagnosticTemplate } from ".";

export const missingInputsTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "`{{{ function }}}` is missing {{{ inputs }}}",
            description:
                "Try adding these inputs. If you've already provided them, make sure they're all on one line.",
            help: undefined,
        },
    ],
};
