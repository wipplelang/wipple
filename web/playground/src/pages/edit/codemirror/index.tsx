import { forwardRef, useEffect, useImperativeHandle, useRef } from "react";
import { EditorView, minimalSetup } from "codemirror";
import { placeholder, keymap, dropCursor } from "@codemirror/view";
import { Compartment, EditorSelection, EditorState, Extension } from "@codemirror/state";
import { defaultKeymap, indentWithTab } from "@codemirror/commands";
import { closeBrackets, closeBracketsKeymap } from "@codemirror/autocomplete";
import { indentOnInput } from "@codemirror/language";
import { wippleLanguage } from "./language";
import { ThemeConfig, theme, themeFromConfig } from "./theme";
import { displayHelp, displayHelpFromEnabled } from "./help";
import { Help } from "../../../models";
import { diagnostics, diagnosticsFromConfig } from "./diagnostics";
import { AssetClickHandler, assets, assetsFromConfig } from "./assets";
import { RenderedDiagnostic, RenderedHighlight } from "wipple-render";

export interface CodeMirrorProps {
    children: string;
    onChange: (value: string) => void;
    autoFocus: boolean;
    onFocus?: () => void;
    onBlur?: () => void;
    onDrop?: () => void;
    lookUpEnabled: boolean;
    onClickLookUp: (help: Help) => void;
    help: (position: number, code: string) => Help | undefined;
    onClickAsset: AssetClickHandler;
    readOnly: boolean;
    diagnostics: RenderedDiagnostic[];
    highlightItems: Record<string, RenderedHighlight>;
    theme: ThemeConfig;
}

export interface CodeMirrorRef {
    editorView: EditorView;
}

const editable = new Compartment();

const editableFromConfig = (config: { readOnly: boolean; onDrop?: () => void }): Extension => [
    EditorView.editable.of(!config.readOnly),
    config.readOnly ? [] : dragAndDrop(config.readOnly),
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

                    displayHelp.of(
                        displayHelpFromEnabled(
                            props.lookUpEnabled,
                            props.theme,
                            props.help,
                            props.highlightItems,
                            props.onClickLookUp,
                        ),
                    ),

                    diagnostics.of(diagnosticsFromConfig({ diagnostics: props.diagnostics })),

                    assets.of(
                        assetsFromConfig({
                            disabled: props.lookUpEnabled,
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

                    placeholder("Write your code here!"),

                    editable.of(
                        editableFromConfig({
                            readOnly: props.readOnly,
                            onDrop: props.onDrop,
                        }),
                    ),

                    EditorView.updateListener.of((update) => {
                        if (update.docChanged) {
                            props.onChange(update.state.doc.toString());
                        }

                        if (
                            update.transactions.some((transaction) =>
                                transaction.isUserEvent("wipple.drop"),
                            )
                        ) {
                            props.onDrop?.();
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
                    onDrop: props.onDrop,
                }),
            ),
        });
    }, [props.readOnly, props.onDrop]);

    useEffect(() => {
        editorView.current!.dispatch({
            effects: theme.reconfigure(themeFromConfig(props.theme)),
        });
    }, [props.theme]);

    useEffect(() => {
        editorView.current!.dispatch({
            effects: displayHelp.reconfigure(
                displayHelpFromEnabled(
                    props.lookUpEnabled,
                    props.theme,
                    props.help,
                    props.highlightItems,
                    props.onClickLookUp,
                ),
            ),
        });
    }, [props.lookUpEnabled, props.theme, props.help, props.highlightItems, props.onClickLookUp]);

    useEffect(() => {
        editorView.current!.dispatch({
            effects: diagnostics.reconfigure(
                diagnosticsFromConfig({ diagnostics: props.diagnostics }),
            ),
        });
    }, [props.diagnostics]);

    useEffect(() => {
        editorView.current!.dispatch({
            effects: assets.reconfigure(
                assetsFromConfig({
                    disabled: props.lookUpEnabled,
                    onClick: props.onClickAsset,
                    highlightItems: props.highlightItems,
                    theme: props.theme,
                }),
            ),
        });
    }, [props.lookUpEnabled, props.onClickAsset, props.highlightItems, props.theme]);

    useEffect(() => {
        if (props.autoFocus) {
            requestAnimationFrame(() => {
                editorView.current!.focus();
            });
        }
    }, [props.autoFocus]);

    return <div ref={containerRef} />;
});

const dragAndDrop = (readOnly: boolean) => [
    EditorView.domEventHandlers({
        dragover: (event) => event.preventDefault(),
        drop: (event, view) => {
            if (readOnly) {
                return;
            }

            if (!event.dataTransfer) {
                return;
            }

            let snippet = event.dataTransfer.getData("wipple/snippet");
            if (!snippet) {
                return;
            }

            const position = view.posAtCoords({ x: event.clientX, y: event.clientY }, false);

            if (
                !view.state.selection.main.empty &&
                position >= view.state.selection.main.from &&
                position <= view.state.selection.main.to
            ) {
                snippet = snippet.replace(
                    "_",
                    view.state.sliceDoc(
                        view.state.selection.main.from,
                        view.state.selection.main.to,
                    ),
                );

                view.dispatch({
                    changes: {
                        from: view.state.selection.main.from,
                        to: view.state.selection.main.to,
                        insert: snippet,
                    },
                    userEvent: "wipple.drop",
                    selection: EditorSelection.cursor(view.state.selection.main.to),
                });
            } else {
                const replace = snippet.includes("_");
                snippet = snippet.replace("_", view.state.sliceDoc());

                if (replace) {
                    view.dispatch({
                        changes: {
                            from: 0,
                            to: view.state.doc.length,
                            insert: snippet,
                        },
                        userEvent: "wipple.drop",
                    });
                } else {
                    let leftPadding = "";
                    const rightPadding = "\n";
                    let endOfLine = view.state.doc.lineAt(position).to;
                    if (endOfLine === view.state.doc.length) {
                        leftPadding = "\n";
                    } else {
                        endOfLine += 1;
                    }

                    view.dispatch({
                        changes: {
                            from: endOfLine,
                            to: endOfLine,
                            insert: leftPadding + snippet + rightPadding,
                        },
                        userEvent: "wipple.drop",
                    });
                }
            }
        },
    }),
    dropCursor(),
];
