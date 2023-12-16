import React, { useEffect, useRef, useState } from "react";
import ReactMarkdown from "react-markdown";
import remarkMath from "remark-math";
import remarkGfm from "remark-gfm";
import rehypeKatex from "rehype-katex";
import remarkSmartypants from "remark-smartypants";
import rehypeRaw from "rehype-raw";
import { TextareaAutosize } from "@mui/material";
import { EditCommands } from "../app";
import { useRefState } from "shared";

export interface TextEditorProps {
    content: string;
    onChange: (content: string) => void;
    isLocked: boolean;
    onFocus: (commands: EditCommands) => void;
    onBlur: (e: FocusEvent) => void;
}

export const TextEditor = (props: TextEditorProps) => {
    const [isEditing, setEditing] = useRefState(false);

    const containerRef = useRef<HTMLDivElement>(null);
    const editorRef = useRef<HTMLTextAreaElement>(null);

    useEffect(() => {
        if (isEditing.current) {
            editorRef.current!.focus();
        }
    }, [isEditing.current]);

    const handleClick: React.MouseEventHandler = (e) => {
        if (props.isLocked) return;

        setEditing(true);

        props.onFocus({
            cut: () => {
                if (!isEditing.current) return undefined;

                const editor = editorRef.current!;

                const text = editor.value.slice(editor.selectionStart, editor.selectionEnd);

                editor.value =
                    editor.value.slice(0, editor.selectionStart) +
                    editor.value.slice(editor.selectionEnd);

                return text;
            },
            copy: () => {
                if (!isEditing.current) return undefined;

                const editor = editorRef.current!;

                const text = editor.value.slice(editor.selectionStart, editor.selectionEnd);

                return text;
            },
            paste: (text) => {
                if (!isEditing.current) return;

                const editor = editorRef.current!;

                editor.value =
                    editor.value.slice(0, editor.selectionStart) +
                    text +
                    editor.value.slice(editor.selectionEnd);

                return text;
            },
            selectAll: () => {
                if (!isEditing.current) return;

                document.execCommand("selectAll");
            },
        });
    };

    return (
        <div className="relative p-4" ref={containerRef}>
            {isEditing.current ? (
                <TextareaAutosize
                    ref={editorRef}
                    className="w-full focus:outline-none resize-none mt-1 dark:bg-inherit dark:text-white"
                    style={{
                        fontFamily: "'JetBrains Mono', monospace",
                        fontVariantLigatures: "none",
                    }}
                    value={props.content}
                    onChange={(e) => {
                        props.onChange(e.target.value);
                    }}
                    onBlur={(e) => {
                        props.onBlur(e.nativeEvent);
                        setEditing(false);
                    }}
                />
            ) : (
                <div onClick={handleClick}>
                    <ReactMarkdown
                        className={`prose prose-sky dark:prose-invert max-w-none ${
                            props.content === "" ? "text-gray-400 pointer-events-none" : ""
                        }`}
                        remarkPlugins={[remarkMath, remarkGfm, remarkSmartypants]}
                        rehypePlugins={[rehypeRaw, rehypeKatex]}
                    >
                        {props.content || "*Write your text here!*"}
                    </ReactMarkdown>
                </div>
            )}
        </div>
    );
};
