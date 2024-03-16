import ReactMarkdown from "react-markdown";
import rehypeHighlight from "rehype-highlight";

const languages: Record<string, import("lowlight").LanguageFn> = {
    wipple: (hljs) => ({
        name: "Wipple",
        contains: [
            {
                className: "comment",
                match: /\-\-.*/,
            },
            {
                className: "number",
                match: /-?[0-9]+(\.[0-9]+)?/,
            },
            {
                className: "operator",
                match: /as|to|by|\^|\*|\/|%|\+|-|<|<=|>|>=|=|\/=|is|and|or|\.|->|:|::|=>/,
            },
            {
                className: "keyword",
                match: /\b(_|!|when|where|type|trait|instance|intrinsic|infer|do)\b/,
            },
            {
                className: "typeName",
                match: /\b([A-Z][A-Za-z0-9\-_]+[?]?)\b/,
            },
            {
                className: "name",
                match: /\b([A-Za-z0-9\-_]+[?]?)\b/,
            },
            {
                className: "string",
                match: /"(?:[^"\\]|\\.)*"/,
            },
        ],
    }),
};

export const Markdown = (props: { children: string }) => (
    <ReactMarkdown
        className="markdown"
        rehypePlugins={[[rehypeHighlight, { languages, prefix: "tok-" }]]}
    >
        {props.children}
    </ReactMarkdown>
);
