import { forwardRef, useEffect, useImperativeHandle, useMemo, useRef } from "react";
import { EditorView, minimalSetup } from "codemirror";
import { placeholder, keymap, dropCursor } from "@codemirror/view";
import { Compartment, EditorState, Extension } from "@codemirror/state";
import { defaultKeymap, indentWithTab } from "@codemirror/commands";
import { closeBrackets, closeBracketsKeymap } from "@codemirror/autocomplete";
import { wippleLanguage } from "./language";
import { ThemeConfig, theme, themeFromConfig } from "./theme";
import { displayHelp, displayHelpFromEnabled } from "./help";
import { Diagnostic, Help } from "../../../models";
import { diagnostics, diagnosticsFromConfig } from "./diagnostics";
import { assets, assetsFromConfig } from "./assets";

export interface CodeMirrorProps {
    children: string;
    onChange: (value: string) => void;
    autoFocus: boolean;
    quickHelpEnabled: boolean;
    onClickQuickHelp: (help: Help) => void;
    help: (code: string) => Help | undefined;
    onClickAsset: (type: string, value: string) => void;
    readOnly: boolean;
    diagnostics: Diagnostic[];
    theme: ThemeConfig;
}

export interface CodeMirrorRef {
    editorView: EditorView;
}

const editable = new Compartment();

const editableFromConfig = (config: { readOnly: boolean }): Extension => [
    EditorView.editable.of(!config.readOnly),
    config.readOnly
        ? []
        : [
              EditorView.domEventHandlers({
                  dragover: (event) => event.preventDefault(),
                  drop: (event, view) => {
                      if (config.readOnly) {
                          return;
                      }

                      if (!event.dataTransfer) {
                          return;
                      }

                      const asset = event.dataTransfer.getData("wipple/asset");
                      if (!asset) {
                          return;
                      }

                      const position = view.posAtCoords({ x: event.clientX, y: event.clientY });
                      if (position == null) {
                          return;
                      }

                      const padding = (s: string) => (s === "" || /\s/.test(s) ? "" : " ");
                      const leftPadding = padding(view.state.sliceDoc(position - 1, position));
                      const rightPadding = padding(view.state.sliceDoc(position, position + 1));

                      view.dispatch({
                          changes: {
                              from: position,
                              to: position,
                              insert: `${leftPadding}\`${asset}\`${rightPadding}`,
                          },
                      });
                  },
              }),
              dropCursor(),
          ],
];

export const CodeMirror = forwardRef<CodeMirrorRef, CodeMirrorProps>((props, ref) => {
    const editorView = useMemo(() => {
        type EditorViewConfig = ConstructorParameters<typeof EditorView>[0] & {};

        const config: EditorViewConfig = {
            state: EditorState.create({
                doc: props.children,
                extensions: [
                    minimalSetup,

                    wippleLanguage,
                    theme.of(themeFromConfig(props.theme)),

                    displayHelp.of(
                        displayHelpFromEnabled(
                            props.quickHelpEnabled ?? false,
                            props.theme,
                            props.help,
                            props.onClickQuickHelp,
                        ),
                    ),

                    diagnostics.of(diagnosticsFromConfig({ diagnostics: props.diagnostics })),

                    assets.of(assetsFromConfig({ onClick: props.onClickAsset })),

                    EditorView.lineWrapping,
                    EditorState.allowMultipleSelections.of(false),

                    keymap.of([...defaultKeymap, indentWithTab, ...closeBracketsKeymap]),
                    closeBrackets(),

                    placeholder("Write your code here!"),

                    editable.of(editableFromConfig({ readOnly: props.readOnly })),

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
            effects: editable.reconfigure(editableFromConfig({ readOnly: props.readOnly })),
        });
    }, [editorView, props.readOnly]);

    useEffect(() => {
        editorView.dispatch({
            effects: theme.reconfigure(themeFromConfig(props.theme)),
        });
    }, [editorView, props.theme]);

    useEffect(() => {
        editorView.dispatch({
            effects: displayHelp.reconfigure(
                displayHelpFromEnabled(
                    props.quickHelpEnabled,
                    props.theme,
                    props.help,
                    props.onClickQuickHelp,
                ),
            ),
        });
    }, [editorView, props.quickHelpEnabled, props.theme]);

    useEffect(() => {
        editorView.dispatch({
            effects: diagnostics.reconfigure(
                diagnosticsFromConfig({ diagnostics: props.diagnostics }),
            ),
        });
    }, [editorView, props.diagnostics]);

    useEffect(() => {
        editorView.dispatch({
            effects: assets.reconfigure(assetsFromConfig({ onClick: props.onClickAsset })),
        });
    }, [editorView, props.onClickAsset]);

    useEffect(() => {
        if (props.autoFocus) {
            requestAnimationFrame(() => {
                editorView.focus();
            });
        }
    }, [editorView, props.autoFocus]);

    return <div ref={containerRef} />;
});
