import { LanguageFn } from "highlight.js";

[
    [/\[:|:\]|[()\[\]{}]/, "@brackets"],
    [/--.*/, "comment"],
    [/:|::|->|=>/, "operator"],
    [/['\/]/, "delimiter"],
    [/_|use|when|type|trait|instance|where|external/, "keyword"],
    [/-?[0-9]+(\.[0-9]+)?/, "number"],
    [/"[^"\\]*(?:\\.[^"\\]*)*"/s, "string"],
    [/[A-Z][^\r\n\t \(\)\[\]\{\}'"/]*/, "type"],
    [/[^\r\n\t \(\)\[\]\{\}'"/]+/, "name"],
];

const wipple: LanguageFn = (hljs) => ({
    keywords: "_ use when type trait instance where external",
    contains: [
        hljs.COMMENT("--", "$"),
        {
            scope: "operator",
            begin: /:|::|->|=>|~>|\.|\|/,
        },
        {
            scope: "delimiter",
            begin: /['\/]/,
        },
        {
            scope: "number",
            begin: /-?[0-9]+(\.[0-9]+)?/,
        },
        {
            scope: "title",
            begin: /[A-Z][^\r\n\t \(\)\[\]\{\}'"/]*/,
        },
        {
            scope: "string",
            begin: '"',
            end: '"',
            contains: [{ begin: "\\\\." }],
        },
    ],
});

export default wipple;
