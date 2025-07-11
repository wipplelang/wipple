import { forwardRef, useEffect, useImperativeHandle, useMemo, useRef } from "react";
import { EditorView, minimalSetup } from "codemirror";
import { placeholder, keymap, Rect } from "@codemirror/view";
import { Compartment, EditorState, Extension } from "@codemirror/state";
import { defaultKeymap, indentWithTab } from "@codemirror/commands";
import { closeBrackets, closeBracketsKeymap } from "@codemirror/autocomplete";
import { indentOnInput } from "@codemirror/language";
import { wippleLanguage } from "./language";
import { ThemeConfig, theme, themeFromConfig } from "./theme";
import { diagnostics, highlightedCodeFromConfig } from "./diagnostics";
import { AssetClickHandler, assets, assetsFromConfig } from "./assets";

export interface Snippet {
    code: string;
    replace: boolean;
}

export interface CodeMirrorProps {
    children: string;
    onChange?: (value: string) => void;
    onChangeSelection?: (selection: { start: number; end: number }) => void;
    autoFocus: boolean;
    onFocus?: () => void;
    onBlur?: () => void;
    onClickAsset?: AssetClickHandler;
    readOnly: boolean;
    highlightedCode?: HighlightedCode;
    highlightItems: Record<string, any>;
    theme: ThemeConfig;
}

export interface HighlightedCode {
    startIndex: number;
    endIndex: number;
    severity: "error" | "warning";
}

export interface CodeMirrorRef {
    editorView: EditorView;
}

const editable = new Compartment();

const editableFromConfig = (config: { readOnly: boolean }): Extension => [
    EditorView.editable.of(!config.readOnly),
];

export const CodeMirror = forwardRef<CodeMirrorRef, CodeMirrorProps>((props, ref) => {
    const containerRef = useRef<HTMLDivElement>(null);
    const editorView = useRef<EditorView | null>(null);

    useEffect(() => {
        type EditorViewConfig = ConstructorParameters<typeof EditorView>[0] & {};

        const config: EditorViewConfig = {
            parent: containerRef.current!,
            state: EditorState.create({
                doc: props.children,
                extensions: [
                    minimalSetup,

                    wippleLanguage,
                    theme.of(themeFromConfig(props.theme)),

                    diagnostics.of(
                        highlightedCodeFromConfig({ highlightedCode: props.highlightedCode }),
                    ),

                    assets.of(
                        assetsFromConfig({
                            disabled: props.readOnly,
                            onClick: props.onClickAsset,
                            highlightItems: props.highlightItems,
                            theme: props.theme,
                        }),
                    ),

                    EditorView.lineWrapping,
                    EditorState.allowMultipleSelections.of(false),

                    keymap.of([...defaultKeymap, indentWithTab, ...closeBracketsKeymap]),
                    closeBrackets(),
                    indentOnInput(),

                    placeholder("Type or drag your code here..."),

                    editable.of(
                        editableFromConfig({
                            readOnly: props.readOnly,
                        }),
                    ),

                    EditorView.updateListener.of((update) => {
                        if (update.docChanged) {
                            props.onChange?.(update.state.doc.toString());
                        }

                        if (update.selectionSet) {
                            const { from, to } = update.state.selection.main;
                            props.onChangeSelection?.({ start: from, end: to });
                        }

                        if (update.focusChanged) {
                            if (update.view.hasFocus) {
                                props.onFocus?.();
                            } else {
                                props.onBlur?.();
                            }
                        }
                    }),
                ],
            }),
        };

        editorView.current = new EditorView(config);

        containerRef.current!.draggable = true;

        return () => {
            editorView.current?.destroy();
        };
    }, []);

    useImperativeHandle(ref, () => ({
        editorView: editorView.current!,
    }));

    useEffect(() => {
        if (editorView.current!.state.doc.toString() !== props.children) {
            editorView.current!.dispatch({
                changes: {
                    from: 0,
                    to: editorView.current!.state.doc.length,
                    insert: props.children,
                },
            });
        }
    }, [props.children]);

    useEffect(() => {
        editorView.current!.dispatch({
            effects: editable.reconfigure(
                editableFromConfig({
                    readOnly: props.readOnly,
                }),
            ),
        });
    }, [props.readOnly]);

    useEffect(() => {
        editorView.current!.dispatch({
            effects: theme.reconfigure(themeFromConfig(props.theme)),
        });
    }, [props.theme]);

    useEffect(() => {
        editorView.current!.dispatch({
            effects: diagnostics.reconfigure(
                highlightedCodeFromConfig({ highlightedCode: props.highlightedCode }),
            ),
        });
    }, [props.highlightedCode]);

    useEffect(() => {
        editorView.current!.dispatch({
            effects: assets.reconfigure(
                assetsFromConfig({
                    disabled: props.readOnly,
                    onClick: props.onClickAsset,
                    highlightItems: props.highlightItems,
                    theme: props.theme,
                }),
            ),
        });
    }, [props.readOnly, props.onClickAsset, props.highlightItems, props.theme]);

    useEffect(() => {
        if (props.autoFocus) {
            requestAnimationFrame(() => {
                editorView.current!.focus();
            });
        }
    }, [props.autoFocus]);

    return <div ref={containerRef} />;
});

export { getTokenAtPos } from "./token";
