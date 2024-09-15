import { DiagnosticTemplate } from ".";

export const wrapperExpectsASinglePatternTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Expected a single pattern here, but found more than one",
            description:
                "Double-check your parentheses and make sure you're using the right pattern here.",
            help: undefined,
        },
    ],
};
