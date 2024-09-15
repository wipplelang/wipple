import { DiagnosticTemplate } from ".";

export const invalidMutatePatternTemplate: DiagnosticTemplate = {
    variants: [
        {
            title: "Can't use `!` here",
            description:
                "You can only use `!` to mutate an existing variable on the left-hand side of the `:`. Putting `!` inside complex patterns isn't supported yet.",
            help: undefined,
        },
    ],
};
