import ReactMarkdown from "react-markdown";

export const Markdown = (props: { children: string }) => (
    <ReactMarkdown className="markdown">{props.children}</ReactMarkdown>
);
