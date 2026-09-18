<script module lang="ts">
    import type * as wipple from "wipple";
    import type { Groups } from "@/models/Groups";

    export const defaultFontSize = 16;
    export const lineHeightRatio = 1.5;
    export const lineSpacingRatio = 0.375;

    export const createGroups = (
        locations: wipple.DiagnosticLocation[],
        { primary }: { primary: "first" | boolean },
    ) => {
        const groups: Groups = {};

        for (const [index, { start, end, group }] of (locations ?? []).entries()) {
            if (group === -1) {
                continue;
            }

            const id = `group${group}`;

            (groups[id] ??= { locations: [] }).locations.push({
                start,
                end,
                primary: primary === "first" ? index === 0 : primary,
            });
        }

        return groups;
    };
</script>

<script lang="ts">
    import {
        elementDecoration,
        markRegex,
        markDecoration,
        accessoryDecoration,
        markRange,
        lineDecoration,
        blockDecoration,
    } from "@/assets/decorations";
    import tokens, {
        enableHighlightingBefore,
        disableHighlightingAfter,
        tokensRegex,
    } from "@/tokens";
    import { stringifyAsset, type Asset } from "@/models/Asset";
    import runtimes from "@/runtimes";
    import widgets, { type WidgetType } from "@/widgets";
    import NumberWidget from "@/widgets/NumberWidget.svelte";
    import { defaultKeymap, indentWithTab } from "@codemirror/commands";
    import { Compartment, EditorState, RangeSet } from "@codemirror/state";
    import { EditorView, keymap, placeholder, ViewPlugin, type Command } from "@codemirror/view";
    import { minimalSetup } from "codemirror";
    import type { Action } from "svelte/action";
    import { type Command as CommandType } from "@/models/Command";
    import { compilerWorker, context } from "@/context.svelte";
    import { nanoid } from "nanoid";
    import Tooltip from "./Tooltip.svelte";
    import CodeEditor from "./CodeEditor.svelte";
    import DiagnosticWidget from "@/widgets/DiagnosticWidget.svelte";

    interface Props {
        readOnly?: boolean;
        code: string;
        groups?: Groups;
        diagnostic?: {
            value: { locations: wipple.DiagnosticLocation[] };
            onclose?: () => void;
        };
        runningLine?: number;
        padding?: string;
        fontSize?: number;
    }

    let {
        readOnly = false,
        code = $bindable(),
        groups = {},
        diagnostic,
        runningLine,
        padding,
        fontSize = defaultFontSize,
    }: Props = $props();

    const id = `code-editor-${nanoid()}`;

    const playground = $derived(context.playground);
    const ideInfo = $derived(context.ideInfo);

    const highlights = $derived(
        (ideInfo ?? []).reduce((highlights, info) => {
            Object.assign(highlights, info.highlights ?? {});
            return highlights;
        }, {}),
    );

    const lineHeight = $derived(fontSize * lineHeightRatio);
    const lineSpacing = $derived(fontSize * lineSpacingRatio);

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
                markRunningLine.of([]),
                markDiagnostic.of([]),
                markGroups.of([]),
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
            const { code: formatted } = await compilerWorker.format({ code });

            if (formatted == null) {
                return;
            }

            // Ensure the formatted code doesn't overwrite new changes
            if (code !== editorView.state.sliceDoc()) {
                return;
            }

            editorView.dispatch({
                changes: { from: 0, to: code.length, insert: formatted },
            });
        })();
    };

    // MARK: - Highlight tokens

    const markTokens = markRegex(new RegExp(tokensRegex, "g"), (match) => {
        const [token] =
            (match.groups &&
                Object.entries(match.groups).find(([_, value]) => value !== undefined)) ??
            [];

        if (!token) {
            return [];
        }

        return [{ decoration: () => markDecoration(`token-${token}`) }];
    });

    // MARK: - Highlight numbers

    const markNumbers = new Compartment();

    const createMarkNumbers = () =>
        markRegex(
            new RegExp(
                `\\((${tokens.number.regex.source})` +
                    / +/.source +
                    `(${tokens.lowercaseName.regex.source})\\)`,
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

    // MARK: - Highlight assets

    const markAssets = markRegex(
        new RegExp(
            `\\((${tokens.lowercaseName.regex.source})` +
                / */.source +
                `(${tokens.text.regex.source})\\)`,
            "g",
        ),
        ([text, type, propsString], view) => {
            if (!(type in widgets)) {
                return [];
            }

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
                        const element = new widgets[type as WidgetType]();

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
        markRegex(new RegExp(tokens.lowercaseName.regex, "g"), (match, view, from, to) => {
            const [name] = match;

            // Don't highlight nested names
            const line = view.state.doc.lineAt(from);
            const before = view.state.doc.slice(line.from, from).toString();
            const after = view.state.doc.slice(to, line.to).toString();
            if (!enableHighlightingBefore.test(before) || disableHighlightingAfter.test(after)) {
                return [];
            }

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

    // MARK: - Highlight running line

    const markRunningLine = new Compartment();

    const createMarkRunningLine = (line: number | undefined) => {
        if (line == null || line > editorView.state.doc.lines) {
            return [];
        }

        const { from } = editorView.state.doc.line(line);

        return [markRange(from, from, () => lineDecoration("running-line"))];
    };

    // MARK: - Display diagnostic

    const diagnosticLine = $derived.by(() => {
        if (!diagnostic) {
            return undefined;
        }

        try {
            return editorView.state.doc.lineAt(diagnostic.value.locations[0].end).number;
        } catch {
            // Position no longer valid
            return undefined;
        }
    });

    const markDiagnostic = new Compartment();

    const createDiagnosticWidget = ({ value, onclose }: NonNullable<typeof diagnostic>) => {
        const diagnosticWidget = new DiagnosticWidget.element!();
        Object.assign(diagnosticWidget, { diagnostic: value, onclose });

        let pos: number;
        try {
            pos = editorView.state.doc.line(diagnosticLine!).to;
        } catch {
            // Position no longer valid; close the diagnostic
            onclose?.();
            return [];
        }

        return EditorView.decorations.of(
            RangeSet.of([blockDecoration(diagnosticWidget).range(pos)]),
        );
    };

    let hoverState = $state<{ element: HTMLElement; labels: string[] }>();

    const getMarkGroupDecoration = (options: {
        start: number;
        end: number;
        group: string;
        labels: string[] | undefined;
        primary: boolean;
    }) => {
        if (
            options.start >= options.end ||
            options.start > editorView.state.doc.length ||
            options.end > editorView.state.doc.length
        ) {
            return undefined;
        }

        const decorationId = nanoid();

        const attributes = {
            "data-group-decoration-id": decorationId,
            "data-group-label": options.group,
        };

        const decoration = markDecoration(
            `group ${options.primary ? "group-primary group-highlighted" : "group-dimmed"} ${diagnostic ? "group-underlined" : ""}`,
            "",
            attributes,
        );

        requestAnimationFrame(() => {
            const element = document.querySelector(
                `[data-group-decoration-id="${decorationId}"]`,
            ) as HTMLElement;

            if (element == null) return;

            element.addEventListener("mouseover", (e) => {
                e.stopPropagation();

                context.highlightedGroup = options.group;

                if (options.labels != null) {
                    hoverState = { element, labels: options.labels };
                }
            });

            element.addEventListener("mouseout", (e) => {
                e.stopPropagation();

                context.highlightedGroup = undefined;
                hoverState = undefined;
            });
        });

        return decoration.range(options.start, options.end);
    };

    $effect(() => {
        const allMarkGroupDecorations = () =>
            [...document.querySelectorAll<HTMLElement>(`#${id} [data-group-decoration-id]`)]
                .map((element) => ({ element, label: element.dataset.groupLabel! }))
                .filter(({ label }) => label != null);

        if (context.highlightedGroup != null) {
            for (const { element, label } of allMarkGroupDecorations()) {
                element.classList.remove("group-highlighted", "group-dimmed");

                if (label === context.highlightedGroup) {
                    element.classList.add("group-highlighted");
                } else {
                    element.classList.add("group-dimmed");
                }
            }
        } else {
            for (const { element } of allMarkGroupDecorations()) {
                if (element.classList.contains("group-primary")) {
                    element.classList.add("group-highlighted");
                    element.classList.remove("group-dimmed");
                } else {
                    element.classList.add("group-dimmed");
                    element.classList.remove("group-highlighted");
                }
            }
        }
    });

    const createMarkGroups = () => {
        const decorations = Object.entries(groups).flatMap(([group, { labels, locations }]) =>
            locations.flatMap(({ start, end, primary = false }) => {
                const decoration = getMarkGroupDecoration({
                    start,
                    end,
                    group,
                    labels,
                    primary,
                });

                return decoration != null ? [decoration] : [];
            }),
        );

        return ViewPlugin.fromClass(class {}, {
            decorations: () => RangeSet.of(decorations, true),
        });
    };

    const markGroups = new Compartment();

    $effect(() => {
        code; // required to update the position of markings
        diagnostic;
        highlights;
        runningLine;
        groups;

        editorView.dispatch({
            effects: [
                markNumbers.reconfigure(createMarkNumbers()),
                markNames.reconfigure(createMarkNames(highlights)),
                markRunningLine.reconfigure(createMarkRunningLine(runningLine)),
                markDiagnostic.reconfigure(
                    diagnostic != null ? createDiagnosticWidget(diagnostic) : [],
                ),
                markGroups.reconfigure([createMarkGroups()]),
            ],
        });

        hoverState = undefined;
    });
</script>

<div
    {id}
    use:codemirror
    class={["code-editor h-full", diagnostic ? "has-diagnostic" : ""]}
    style:--code-editor-padding={padding}
    style:--code-editor-font-size="{fontSize}px"
    style:--code-editor-line-height="{lineHeight}px"
    style:--code-editor-line-spacing="{lineSpacing}px"
></div>

{#if hoverState != null}
    {@const { element, labels } = hoverState}

    <Tooltip reference={element} delay={500}>
        {#snippet content()}
            <div class="flex flex-row items-baseline gap-[4pt]">
                <CodeEditor readOnly code={labels.join(" or ")} />
            </div>
        {/snippet}
    </Tooltip>
{/if}
