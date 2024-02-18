import { useEffect, useMemo, useRef } from "react";
import { EditorView, minimalSetup } from "codemirror";
import { placeholder, keymap } from "@codemirror/view";
import { Compartment, EditorState } from "@codemirror/state";
import { defaultKeymap, indentWithTab } from "@codemirror/commands";
import { closeBrackets, closeBracketsKeymap } from "@codemirror/autocomplete";
import { wippleLanguage } from "./language";
import { ThemeConfig, theme, themeFromConfig } from "./theme";
import { selectionMode, selectionModeFromEnabled } from "./mode";
import { Diagnostic } from "../../../models";
import { highlight, highlightFromDiagnostics } from "./highlight";

export interface CodeMirrorProps {
    children: string;
    onChange?: (value: string) => void;
    quickHelpEnabled?: boolean;
    onClickQuickHelp?: (selected: boolean) => void;
    readOnly: boolean;
    diagnostics: Diagnostic[];
    theme: ThemeConfig;
}

const editable = new Compartment();

export const CodeMirror = (props: CodeMirrorProps) => {
    const handleClickQuickHelp = (selected: boolean) => {
        props.onClickQuickHelp?.(selected);
    };

    const editorView = useMemo(() => {
        type EditorViewConfig = ConstructorParameters<typeof EditorView>[0] & {};

        const config: EditorViewConfig = {
            state: EditorState.create({
                doc: props.children,
                extensions: [
                    minimalSetup,

                    wippleLanguage,
                    theme.of(themeFromConfig(props.theme)),

                    selectionMode.of(
                        selectionModeFromEnabled(
                            props.quickHelpEnabled ?? false,
                            props.theme,
                            handleClickQuickHelp,
                        ),
                    ),

                    highlight.of(highlightFromDiagnostics(props.diagnostics)),

                    EditorView.lineWrapping,
                    EditorState.allowMultipleSelections.of(false),

                    keymap.of([...defaultKeymap, indentWithTab, ...closeBracketsKeymap]),
                    closeBrackets(),

                    placeholder("Write your code here!"),

                    editable.of(EditorView.editable.of(!props.readOnly)),

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
            effects: editable.reconfigure(EditorView.editable.of(!props.readOnly)),
        });
    }, [editorView, props.readOnly]);

    useEffect(() => {
        editorView.dispatch({
            effects: theme.reconfigure(themeFromConfig(props.theme)),
        });
    }, [editorView, props.theme]);

    useEffect(() => {
        editorView.dispatch({
            effects: selectionMode.reconfigure(
                selectionModeFromEnabled(
                    props.quickHelpEnabled ?? false,
                    props.theme,
                    handleClickQuickHelp,
                ),
            ),
        });
    }, [editorView, props.quickHelpEnabled, props.theme]);

    useEffect(() => {
        editorView.dispatch({
            effects: highlight.reconfigure(highlightFromDiagnostics(props.diagnostics)),
        });
    }, [editorView, props.diagnostics]);

    return <div ref={containerRef} />;
};
