import { DiagnosticTemplate } from ".";

export const cannotCollectTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Can't create {{{ container }}} from {{{ element }}}",
            description:
                "`collect`ing a sequence of {{{ element }}} into {{{ container }}} isn't supported.",
            help: undefined,
        },
    ],
};
