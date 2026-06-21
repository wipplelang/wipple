import tokens from "../../../../playground/src/tokens";

hljs.registerLanguage("wipple", (hljs) => ({
    contains: Object.values(tokens).map((token) => ({
        className: token.highlight,
        begin: token.regex,
    })),
}));

hljs.initHighlightingOnLoad();
