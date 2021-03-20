import App from "./App.svelte";
import Prism from "prismjs";

// @ts-ignore
import interpreterWasm from "../../interpreter/Cargo.toml";

interpreterWasm().then((interpreter) => {
    new App({
        target: document.body,
        props: { interpreter },
    });
});

Prism.languages.wipple = {
    comment: {
        pattern: /--.*/,
        greedy: true,
    },
    operator: {
        pattern: /[`~!@#$%^&*()\-_=+\\|;:,<.>/?]+(?=[ \t()\[\]{}']|$)/m,
        greedy: true,
    },
    number: {
        pattern: /[0-9]+(\.[0-9]+)?(?=[ \t()\[\]{}']|$)/m,
        greedy: true,
    },
    name: {
        pattern: /[^ \t\n()\[\]{}'"]+/,
        greedy: true,
    },
    string: {
        pattern: /"[^\n"]*"/,
        greedy: true,
    },
    punctuation: /[()\[\]{}']/,
};

import "./prismLineNumbersPlugin.js";
