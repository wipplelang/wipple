import ReactMarkdown from "react-markdown";
import remarkMath from "remark-math";
import rehypeHighlight from "rehype-highlight";
import rehypeKatex from "rehype-katex";

const languages: Record<string, import("lowlight").LanguageFn> = {
    wipple: (_hljs) => ({
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
                match: /\b(_|!|when|where|type|trait|instance|intrinsic|infer|do|default)\b/,
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

export const Markdown = (props: { children: string; enableMath?: boolean; className?: string }) => (
    <ReactMarkdown
        className={`markdown ${props.className ?? ""}`}
        remarkPlugins={[...(props.enableMath ? [remarkMath] : [])]}
        rehypePlugins={[
            [rehypeHighlight, { languages, prefix: "tok-" }],
            ...(props.enableMath ? [rehypeKatex] : []),
        ]}
    >
        {props.children}
    </ReactMarkdown>
);
