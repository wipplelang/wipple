import App from "./App.svelte";
import Prism from "prismjs";

Prism.languages.wipple = {
    comment: {
        pattern: /--.*/,
        greedy: true,
    },
    operator: {
        pattern: /[`~!@#$%^&*()\-_=+|;:,<.>/?]+(?=[ \t()\[\]{}']|$)/m,
        greedy: true,
    },
    number: {
        pattern: /-?[0-9]+(\.[0-9]+)?(?=[ \t()\[\]{}']|$)/m,
        greedy: true,
    },
    name: {
        pattern: /[^ \t\n()\[\]{}'\\"]+/,
        greedy: true,
    },
    string: {
        pattern: /"[^\n"]*"/,
        greedy: true,
    },
    punctuation: /[()\[\]{}'\\]/,
};

import "./prismLineNumbersPlugin.js";

new App({ target: document.body });
