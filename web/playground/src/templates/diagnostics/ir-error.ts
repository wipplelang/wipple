import { DiagnosticTemplate } from ".";

export const irErrorTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Couldn't process this code",
            description:
                "You've found a bug in Wipple — your code is correct, but Wipple couldn't produce IR for it. Please report feedback so this can be fixed!",
            help: undefined,
        },
    ],
};
