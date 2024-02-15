import { useEffect, useMemo, useRef } from "react";
import { EditorView, minimalSetup } from "codemirror";
import { placeholder, keymap } from "@codemirror/view";
import { EditorState } from "@codemirror/state";
import { defaultKeymap, indentWithTab } from "@codemirror/commands";
import { closeBrackets, closeBracketsKeymap } from "@codemirror/autocomplete";
import { wippleLanguage } from "./language";
import { defaultThemeConfig, theme, themeFromConfig } from "./theme";
import { useDarkMode } from "usehooks-ts";

export interface CodeMirrorProps {
    children: string;
    onChange?: (value: string) => void;
}

export const CodeMirror = (props: CodeMirrorProps) => {
    const { isDarkMode } = useDarkMode();

    const editorView = useMemo(() => {
        type EditorViewConfig = ConstructorParameters<typeof EditorView>[0] & {};

        const config: EditorViewConfig = {
            state: EditorState.create({
                doc: props.children,
                extensions: [
                    minimalSetup,

                    wippleLanguage,
                    theme.of(themeFromConfig(defaultThemeConfig())),

                    EditorView.lineWrapping,
                    EditorState.allowMultipleSelections.of(false),

                    keymap.of([...defaultKeymap, indentWithTab, ...closeBracketsKeymap]),
                    closeBrackets(),

                    placeholder("Write your code here!"),

                    EditorView.updateListener.of((update) => {
                        if (update.docChanged) {
                            props.onChange?.(update.state.doc.toString());
                        }
                    }),
                ],
            }),
        };

        return new EditorView(config);
    }, []);

    const containerRef = useRef<HTMLDivElement>(null);

    useEffect(() => {
        if (!containerRef.current) {
            return;
        }

        containerRef.current.appendChild(editorView.dom);

        return () => {
            containerRef.current?.removeChild(editorView.dom);
        };
    }, [containerRef.current]);

    useEffect(() => {
        editorView.dispatch({
            effects: theme.reconfigure(
                themeFromConfig({ ...defaultThemeConfig(), dark: isDarkMode }),
            ),
        });
    }, [editorView, isDarkMode]);

    return <div ref={containerRef}></div>;
};
