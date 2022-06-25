hljs.registerLanguage("wipple", (hljs) => ({
    name: "Wipple",
    aliases: ["wipple", "wpl"],
    keywords: {
        keyword: "_ use when type trait instance where external",
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
