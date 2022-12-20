import React, { useEffect, useRef, useState } from "react";
import ReactMarkdown from "react-markdown";
import remarkMath from "remark-math";
import remarkGfm from "remark-gfm";
import rehypeKatex from "rehype-katex";
import remarkSmartypants from "remark-smartypants";
import rehypeRaw from "rehype-raw";
import { TextareaAutosize } from "@mui/material";

export interface TextEditorProps {
    content: string;
    onChange: (content: string) => void;
    isLocked: boolean;
}

export const TextEditor = (props: TextEditorProps) => {
    const [isEditing, setEditing] = useState(false);
    const [buffer, setBuffer] = useState<string | undefined>();

    const containerRef = useRef<HTMLDivElement>(null);
    const editorRef = useRef<HTMLTextAreaElement>(null);

    useEffect(() => {
        const listener = (e: MouseEvent) => {
            if (document.elementFromPoint(e.clientX, e.clientY) instanceof HTMLAnchorElement) {
                return;
            }

            const willBeginEditing = containerRef.current?.contains(e.target as Node) ?? false;

            if (!props.isLocked || !willBeginEditing) {
                setEditing(willBeginEditing);
            }
        };

        window.addEventListener("click", listener);

        return () => {
            window.removeEventListener("click", listener);
        };
    }, [props.isLocked, setEditing, containerRef]);

    useEffect(() => {
        if (isEditing) {
            setBuffer(props.content);
            editorRef.current!.focus();
        } else if (buffer != null) {
            props.onChange(buffer);
        }
    }, [isEditing]);

    return (
        <div className="relative p-4" ref={containerRef}>
            {isEditing ? (
                <TextareaAutosize
                    ref={editorRef}
                    className="w-full focus:outline-none resize-none mt-1 dark:bg-inherit dark:text-white"
                    style={{
                        fontFamily: "'JetBrains Mono', monospace",
                        fontVariantLigatures: "none",
                    }}
                    value={buffer}
                    onChange={(e) => setBuffer(e.target.value)}
                />
            ) : (
                <ReactMarkdown
                    className={`prose prose-sky dark:prose-invert max-w-none ${
                        props.content === "" ? "text-gray-400 pointer-events-none" : ""
                    }`}
                    remarkPlugins={[remarkMath, remarkGfm, remarkSmartypants]}
                    rehypePlugins={[rehypeRaw, rehypeKatex]}
                >
                    {props.content || "*Write your text here!*"}
                </ReactMarkdown>
            )}
        </div>
    );
};
