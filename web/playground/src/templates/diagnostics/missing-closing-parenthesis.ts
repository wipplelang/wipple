import { DiagnosticTemplate } from ".";

export const missingClosingParenthesisTemplate: DiagnosticTemplate = ({ code }) => ({
    title: "Missing closing `)`",
    description:
        "Every opening parenthesis needs a closing parenthesis. Try adding one at the end.",
    help: {
        type: "choice",
        question: `Does \`${code}\` contain one item or multiple items?`,
        choices: [
            {
                choice: "One",
                response: {
                    type: "message",
                    message: `Try typing a \`)\` immediately after \`${code}\`.`,
                },
            },
            {
                choice: "Multiple",
                response: {
                    type: "prompt",
                    prompt: "Which part of the code is a single item?",
                    response: ({ answer }) => ({
                        type: "message",
                        message: `Try typing a \`)\` immediately after \`${answer}\`. That will treat just that code as one item, and the rest of the code as separate.`,
                    }),
                },
            },
        ],
    },
});
