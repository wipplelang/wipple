<script lang="ts">
    import {
        elementDecoration,
        markRegex,
        markDecoration,
        accessoryDecoration,
        markRange,
        blockDecoration,
    } from "$lib/assets/decorations";
    import tokens from "$lib/assets/tokens";
    import { stringifyAsset, type Asset } from "$lib/models/Asset";
    import runtimes from "$lib/runtimes";
    import widgets, { type WidgetType } from "$lib/widgets";
    import NumberWidget from "$lib/widgets/NumberWidget.svelte";
    import PlaceholderWidget from "$lib/widgets/PlaceholderWidget.svelte";
    import { defaultKeymap, indentWithTab } from "@codemirror/commands";
    import { Compartment, EditorState, Prec, RangeSet } from "@codemirror/state";
    import { EditorView, keymap, placeholder, type Command } from "@codemirror/view";
    import { minimalSetup } from "codemirror";
    import type { Action } from "svelte/action";
    import { type Command as CommandType } from "$lib/models/Command";
    import * as api from "$lib/api";
    import { context } from "$lib/context.svelte";
    import DiagnosticWidget from "$lib/widgets/DiagnosticWidget.svelte";

    interface Props {
        readOnly?: boolean;
        code: string;
        diagnostic?: {
            value: any;
            stale: boolean;
            onclose: () => void;
        };
        padding?: string;
    }

    let { readOnly = false, code = $bindable(), diagnostic, padding }: Props = $props();

    const playground = $derived(context.playground);
    const ideInfo = $derived(context.ideInfo);

    const highlights = $derived(
        (ideInfo ?? []).reduce((highlights, info) => {
            Object.assign(highlights, info.highlights ?? {});
            return highlights;
        }, {}),
    );

    let editorView: EditorView;
    const codemirror: Action = (node) => {
        editorView = new EditorView({
            parent: node,
            doc: code,
            extensions: [
                minimalSetup,
                keymap.of([...defaultKeymap, indentWithTab]),
                EditorState.allowMultipleSelections.of(false),
                markTokens,
                markNumbers.of([]),
                markAssets,
                markNames.of([]),
                markDiagnostic.of([]),
                placeholder("Type or drag your code here..."),
                EditorView.editable.of(!readOnly),
                EditorView.updateListener.of((update) => {
                    if (update.docChanged) {
                        code = update.state.sliceDoc();
                    }
                }),
            ],
        });
    };

    // MARK: - Commands

    export const runCommand = (command: Command) => {
        command(editorView);
    };

    export const getDropParams = (command: CommandType, { x, y }: { x: number; y: number }) => {
        const { top, bottom, left, right } = editorView.contentDOM.getBoundingClientRect();
        const width = editorView.contentDOM.clientWidth;

        const padding = 100;

        if (
            x < left - padding ||
            x > right + padding ||
            y < top - padding ||
            y > bottom + padding
        ) {
            // Out of bounds
            return undefined;
        }

        // Insert at beginning if document is empty
        if (editorView.state.doc.length === 0) {
            return {
                command,
                startLineNumber: 1,
                endLineNumber: 1,
                top: top + "px",
                left: left + "px",
                width: width + "px",
                height: "0",
            };
        }

        const lineHeight = parseFloat(
            window
                .getComputedStyle(editorView.contentDOM)
                .getPropertyValue("--code-editor-line-height")
                .replace(/px$/, ""),
        );

        const lineSpacing = parseFloat(
            window
                .getComputedStyle(editorView.contentDOM)
                .getPropertyValue("--code-editor-line-spacing")
                .replace(/px$/, ""),
        );

        // Choose the closest line, and allow going one past the end
        const startLineNumber =
            Math.min(
                Math.floor(Math.max(y - top, 0) / (lineHeight + lineSpacing)),
                editorView.state.doc.lines,
            ) + 1;

        let endLineNumber = startLineNumber;
        if (command.surround && endLineNumber <= editorView.state.doc.lines) {
            // Select until the next closing brace (or the end of the document)

            const iter = editorView.state.doc.iterRange(
                editorView.state.doc.line(startLineNumber).from,
            );

            let openingBraceCount = 0;
            for (const line of iter) {
                if (iter.lineBreak) {
                    endLineNumber++;
                    continue;
                }

                openingBraceCount += line.match("{")?.length ?? 0;
                openingBraceCount -= line.match("}")?.length ?? 0;

                if (openingBraceCount < 0) {
                    break;
                }
            }

            endLineNumber += openingBraceCount + 1;

            endLineNumber = Math.min(endLineNumber, editorView.state.doc.lines + 1);
        }

        const offset = (lineNumber: number) =>
            `${top + (lineHeight + lineSpacing) * (lineNumber - 1)}px`;

        return {
            command,
            startLineNumber,
            endLineNumber,
            top: offset(startLineNumber),
            left: left + "px",
            width: width + "px",
            height: `calc(${offset(endLineNumber)} - ${offset(startLineNumber)})`,
        };
    };

    export const drop = (params: NonNullable<ReturnType<typeof getDropParams>>) => {
        const { command, startLineNumber, endLineNumber } = params;

        if (command.surround) {
            const from =
                startLineNumber <= editorView.state.doc.lines
                    ? editorView.state.doc.line(startLineNumber).from
                    : editorView.state.doc.length;

            const to =
                endLineNumber <= editorView.state.doc.lines
                    ? editorView.state.doc.line(endLineNumber).to
                    : editorView.state.doc.length;

            const inner = editorView.state.sliceDoc(from, to);

            const before = startLineNumber <= editorView.state.doc.lines ? "" : "\n";

            const code =
                inner.length > 0
                    ? command.surround.before + "\n" + inner + "\n" + command.surround.after
                    : command.surround.before + "\n" + command.surround.after;

            editorView.dispatch({
                changes: { from, to, insert: before + code },
            });
        } else if (startLineNumber <= 1) {
            editorView.dispatch({
                changes: { from: 0, to: 0, insert: command.code + "\n" },
            });
        } else {
            const pos =
                startLineNumber <= editorView.state.doc.lines
                    ? editorView.state.doc.line(startLineNumber - 1).to
                    : editorView.state.doc.length;

            editorView.dispatch({
                changes: { from: pos, to: pos, insert: "\n" + command.code },
            });
        }

        const code = editorView.state.sliceDoc();

        // Format when done
        (async () => {
            const { code: formatted } = await api.format({ code });

            // Ensure the formatted code doesn't overwrite new changes
            if (code !== editorView.state.sliceDoc()) {
                return;
            }

            if (formatted !== code) {
                editorView.dispatch({
                    changes: { from: 0, to: code.length, insert: formatted },
                });
            }
        })();
    };

    // MARK: - Highlight tokens

    const markTokens = Object.entries(tokens).map(([token, regex]) =>
        markRegex(new RegExp(regex, "g"), () => [
            { decoration: () => markDecoration(`token-${token}`) },
        ]),
    );

    // MARK: - Highlight numbers

    const markNumbers = new Compartment();

    const createMarkNumbers = () =>
        markRegex(
            new RegExp(
                `\\((${tokens.number.source})` +
                    / +/.source +
                    `(${tokens.lowercaseName.source})\\)`,
                "g",
            ),
            ([_text, number, unit], view) => {
                if (!playground) {
                    return [];
                }

                const unitInfo = runtimes[playground.runtime].units?.[unit];
                if (!unitInfo) {
                    return [];
                }

                const length = number.length;

                return [
                    {
                        decoration: () => {
                            const element = new NumberWidget.element!();
                            Object.assign(element, {
                                number: parseFloat(number),
                                unit,
                                unitInfo,
                            });

                            element.addEventListener("change", () => {
                                const pos = view.posAtDOM(element);

                                view.dispatch({
                                    changes: {
                                        from: pos - length,
                                        to: pos,
                                        insert: (element as any).number.toString(),
                                    },
                                });
                            });

                            return accessoryDecoration(element);
                        },
                        index: length + 1,
                    },
                ];
            },
        );

    $effect(() => {
        editorView.dispatch({
            effects: markNumbers.reconfigure(createMarkNumbers()),
        });
    });

    // MARK: - Highlight assets

    const markAssets = markRegex(
        new RegExp(
            `\\((${tokens.lowercaseName.source})` + / */.source + `(${tokens.text.source})\\)`,
            "g",
        ),
        ([text, type, propsString], view) => {
            let props: Omit<Asset, "type"> | undefined;
            try {
                props = JSON.parse(propsString.slice(1, -1));
            } catch (error) {
                console.error(error);
                // continue; always replace the JSON with a decoration
            }

            const length = text.length;

            return [
                {
                    decoration: () => {
                        const element =
                            type in widgets
                                ? new widgets[type as WidgetType]()
                                : new PlaceholderWidget.element!();

                        if (props) {
                            Object.assign(element, props);

                            element.addEventListener("change", () => {
                                const newProps: Omit<Asset, "type"> = {};
                                for (const key of Object.keys(props)) {
                                    (newProps as any)[key] = (element as any)[key];
                                }

                                const pos = view.posAtDOM(element);

                                view.dispatch({
                                    changes: {
                                        // Skip the parenthesis
                                        from: pos + 1,
                                        to: pos + length - 1,
                                        insert: stringifyAsset({ type, ...newProps } as any),
                                    },
                                });
                            });
                        }

                        return elementDecoration(element);
                    },
                },
            ];
        },
        { atomic: true },
    );

    // MARK: - Highlight names

    // Because highlights are loaded dynamically, we need to reinitialize the
    // entire extension when they change. After that point, though, creating new
    // highlights is inexpensive (just CSS styles)
    const markNames = new Compartment();

    const createMarkNames = (highlights: Record<string, any>) =>
        markRegex(new RegExp(tokens.lowercaseName, "g"), ([name]) => {
            const highlight = highlights[name];
            if (!highlight) {
                return [];
            }

            // Using the color CSS variable is OK here because all the color
            // classes are already defined in full and will be properly
            // included by Tailwind
            return [
                {
                    decoration: () =>
                        markDecoration(
                            "token-highlighted",
                            `--highlight-color: var(--color-${highlight.color}-500);` +
                                ` --highlight-background-color: var(--color-${highlight.color}-300);` +
                                `${highlight.icon ? ` --highlight-icon: '${highlight.icon}';` : ""}`,
                        ),
                },
            ];
        });

    $effect(() => {
        editorView.dispatch({
            effects: markNames.reconfigure(createMarkNames(highlights)),
        });
    });

    // MARK: - Display diagnostic

    const diagnosticLine = $derived.by(() => {
        if (!diagnostic) {
            return undefined;
        }

        try {
            return editorView.state.doc.lineAt(diagnostic.value.location.end.index).number;
        } catch {
            // Position no longer valid
            return undefined;
        }
    });

    const markDiagnostic = new Compartment();

    const createMarkDiagnostic = ({ value, stale, onclose }: NonNullable<typeof diagnostic>) => {
        const diagnosticWidget = new DiagnosticWidget.element!();
        Object.assign(diagnosticWidget, {
            diagnostic: value,
            stale,
            onclose,
        });

        let pos: number;
        try {
            pos = editorView.state.doc.line(diagnosticLine!).to;
        } catch {
            // Position no longer valid; close the diagnostic
            onclose();
            return [];
        }

        return [
            stale
                ? []
                : Prec.high(
                      markRange(value.location.start.index, value.location.end.index, () =>
                          markDecoration(
                              "bg-blue-500/10 rounded-[6px] underline underline-offset-[2pt] decoration-wavy decoration-blue-500",
                          ),
                      ),
                  ),
            EditorView.decorations.of(RangeSet.of([blockDecoration(diagnosticWidget).range(pos)])),
        ];
    };

    $effect(() => {
        code; // required to update the position of the diagnostic

        editorView.dispatch({
            effects: markDiagnostic.reconfigure(diagnostic ? createMarkDiagnostic(diagnostic) : []),
        });
    });
</script>

<div
    use:codemirror
    class="code-editor h-full w-full"
    style={padding ? `--code-editor-padding: ${padding};` : ""}
></div>
