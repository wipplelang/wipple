import { DiagnosticTemplate } from ".";

export const missingParenthesesAroundOperatorTemplate: DiagnosticTemplate = ({ operator }) => ({
    title: `Missing parentheses around the inputs to ${operator}`,
    description: `Spaces group before operators do, so you need to put parentheses around just the inputs to ${operator}.`,
    help: {
        type: "choice",
        question: "Are you trying to display something on the screen?",
        choices: [
            {
                choice: "Yes",
                response: {
                    type: "message",
                    message: `Put parentheses around just the numbers and the ${operator}. For example, instead of \`show a + b\`, try \`show (a + b)\`.`,
                },
            },
            {
                choice: "No",
                response: {
                    type: "message",
                    message: `Wipple determined that the left side of the ${operator} produces no value, which is why this error is appearing. Make sure you have parentheses around just the code you want to use with ${operator}.`,
                },
            },
        ],
    },
});
