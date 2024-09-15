import { DiagnosticTemplate } from ".";

export const unresolvedInstanceTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Using `{{{ trait }}}` requires that `instance ({{ instance }})` exists",
            description:
                "`{{{ trait }}}` needs this instance to exist so it can use its input correctly. Make sure you're providing the right type of input here, or you can define your own `instance`.",
            help: undefined,
        },
    ],
};
