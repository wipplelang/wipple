import { forwardRef, useEffect, useImperativeHandle, useMemo, useRef } from "react";
import { EditorView, minimalSetup } from "codemirror";
import { placeholder, keymap } from "@codemirror/view";
import { Compartment, EditorState } from "@codemirror/state";
import { defaultKeymap, indentWithTab } from "@codemirror/commands";
import { closeBrackets, closeBracketsKeymap } from "@codemirror/autocomplete";
import { wippleLanguage } from "./language";
import { ThemeConfig, theme, themeFromConfig } from "./theme";
import { selectionMode, selectionModeFromEnabled } from "./mode";
import { Diagnostic, Fix } from "../../../models";
import { highlight, highlightFromDiagnostics } from "./highlight";
import { errors, errorsFromConfig } from "./errors";

export interface CodeMirrorProps {
    children: string;
    onChange: (value: string) => void;
    quickHelpEnabled: boolean;
    onClickQuickHelp: (selected: boolean) => void;
    inErrorMode: boolean;
    readOnly: boolean;
    diagnostics: Diagnostic[];
    theme: ThemeConfig;
}

export interface CodeMirrorRef {
    editorView: EditorView;
}

const language = new Compartment();
const editable = new Compartment();

export const CodeMirror = forwardRef<CodeMirrorRef, CodeMirrorProps>((props, ref) => {
    const applyFix = (fix: Fix) => {
        alert("TODO");
    };

    const editorView = useMemo(() => {
        type EditorViewConfig = ConstructorParameters<typeof EditorView>[0] & {};

        const config: EditorViewConfig = {
            state: EditorState.create({
                doc: props.children,
                extensions: [
                    minimalSetup,

                    language.of(wippleLanguage(props.theme.highlight)),
                    theme.of(themeFromConfig(props.theme)),

                    selectionMode.of(
                        selectionModeFromEnabled(
                            props.quickHelpEnabled ?? false,
                            props.theme,
                            props.onClickQuickHelp,
                        ),
                    ),

                    highlight.of(highlightFromDiagnostics(props.diagnostics)),

                    errors.of(
                        errorsFromConfig({
                            enabled: props.inErrorMode,
                            diagnostics: props.diagnostics,
                            onApplyFix: applyFix,
                            editorView: () => {
                                // HACK for Fast Refresh
                                try {
                                    return editorView;
                                } catch {
                                    return null;
                                }
                            },
                            theme: props.theme,
                        }),
                    ),

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

    useImperativeHandle(ref, () => ({ editorView }), [editorView]);

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
            effects: language.reconfigure(wippleLanguage(props.theme.highlight)),
        });
    }, [editorView, props.theme.highlight]);

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
                    props.onClickQuickHelp,
                ),
            ),
        });
    }, [editorView, props.quickHelpEnabled, props.theme]);

    useEffect(() => {
        editorView.dispatch({
            effects: highlight.reconfigure(highlightFromDiagnostics(props.diagnostics)),
        });
    }, [editorView, props.diagnostics]);

    useEffect(() => {
        editorView.dispatch({
            effects: errors.reconfigure(
                errorsFromConfig({
                    enabled: props.inErrorMode,
                    diagnostics: props.diagnostics,
                    onApplyFix: applyFix,
                    editorView: () => editorView,
                    theme: props.theme,
                }),
            ),
        });
    }, [editorView, props.inErrorMode, props.diagnostics, props.theme]);

    return <div ref={containerRef} />;
});
