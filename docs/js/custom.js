hljs.registerLanguage("wipple", (hljs) => ({
    name: "Wipple",
    aliases: ["wipple"],
    keywords: {
        keyword: "_ ! when where type trait instance intrinsic infer do default",
    },
    contains: [
        {
            className: "comment",
            begin: /--.*/,
        },
        {
            className: "string",
            begin: /"(?:[^"\\]|\\.)*"/,
        },
        {
            className: "type",
            begin: /\b[A-Z][^\r\n\t \(\)\[\]\{\}'"/]*\b/,
        },
        {
            className: "number",
            begin: /\b-?[0-9]+(\.[0-9]+)?\b/,
        },
    ],
}));

hljs.initHighlightingOnLoad();
