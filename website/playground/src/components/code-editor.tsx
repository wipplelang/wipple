import ReactDOM from "react-dom/client";
import React, { useCallback, useEffect, useMemo, useRef, useState } from "react";
import SimpleCodeEditor from "./react-simple-code-editor";
import * as prism from "prismjs";
import ListItemText from "@mui/material/ListItemText";
import Menu from "@mui/material/Menu";
import MenuItem from "@mui/material/MenuItem";
import MenuList from "@mui/material/MenuList";
import Tooltip from "@mui/material/Tooltip";
import useMediaQuery from "@mui/material/useMediaQuery";
import { useSpring, animated } from "react-spring";
import useMeasure from "react-use-measure";
import {
    AnalysisOutputSyntaxHighlightingItem,
    HoverOutput,
    AnalysisOutputSnippets,
    Snippet,
    useRefState,
    Markdown,
    PlaygroundRunner,
    AnalysisOutput,
    Output,
} from "shared";
import SubjectRounded from "@mui/icons-material/SubjectRounded";
import PlayArrowRounded from "@mui/icons-material/PlayArrowRounded";
import PauseRounded from "@mui/icons-material/PauseRounded";
import FullScreenRounded from "@mui/icons-material/FullscreenRounded";
import FullScreenExitRounded from "@mui/icons-material/FullscreenExitRounded";
import MoreHoriz from "@mui/icons-material/MoreHoriz";
import Download from "@mui/icons-material/Download";
import * as Sentry from "@sentry/react";
import PopupState, {
    bindMenu,
    bindPopover,
    bindToggle,
    bindTrigger,
} from "material-ui-popup-state";
import { minimalSetup } from "codemirror";
import { EditorView, placeholder, Decoration, ViewPlugin, DecorationSet } from "@codemirror/view";
import { Compartment, EditorState, Prec, Range } from "@codemirror/state";
import { syntaxTree } from "@codemirror/language";
import { defaultKeymap, indentWithTab } from "@codemirror/commands";
import { keymap, hoverTooltip, ViewUpdate, closeHoverTooltips, WidgetType } from "@codemirror/view";
import { githubLightInit, githubDarkInit } from "@uiw/codemirror-theme-github";
import { styleTags, tags as t } from "@lezer/highlight";
import { LRLanguage, LanguageSupport } from "@codemirror/language";
import { parser } from "../languages/wipple.grammar";
import { EditCommands, Settings } from "../App";
import { ListItemIcon, Popover } from "@mui/material";
import { SwatchesPicker } from "react-color";
import { closeBrackets, closeBracketsKeymap } from "@codemirror/autocomplete";
import {
    RemoteCursor,
    remoteCursors,
    remoteCursorsTheme,
    useCollaboration,
    userColor,
} from "../helpers";
import PeopleAltRounded from "@mui/icons-material/PeopleAltRounded";
import GroupAddRounded from "@mui/icons-material/GroupAddRounded";
import { Piano } from "react-piano";
import { Note } from "./note";
import * as tonal from "tonal";
import emojiRegex from "emoji-regex";
import emojiData from "@emoji-mart/data";
import EmojiPicker from "@emoji-mart/react";
import graphemeSplit from "graphemesplit";
import { SetupIcon } from "./picker";
import { inlineSuggestion } from "./codemirror-extension-inline-suggestion";
import * as commands from "@codemirror/commands";
import AddRoundedIcon from "@mui/icons-material/AddRounded";
import CodeIcon from "@mui/icons-material/Code";
import KeyboardReturnIcon from "@mui/icons-material/KeyboardReturn";

export interface CodeEditorProps {
    id: string;
    code: string;
    autoRun: boolean;
    onChangeAutoRun: (autoRun: boolean) => void;
    lint: boolean;
    setup: string | undefined;
    collapse: boolean;
    onChangeCollapse: (collapse: boolean) => void;
    settings: Settings;
    autoFocus: boolean;
    onChange: (code: string) => void;
    onFocus: (commands: EditCommands) => void;
    onBlur: (e: FocusEvent) => void;
}

interface SpecialSnippet {
    element: () => React.ReactNode;
    name: string;
    code: string;
    lineMode: "before" | "after" | null;
}

const codeEditorFontSize = "16px";

export const CodeEditor = (props: CodeEditorProps) => {
    const containerID = `code-editor-container-${props.id}`;

    const prefersDarkMode = useMediaQuery("(prefers-color-scheme: dark)");

    // Store settings in a ref so that the code editor can access them even
    // after they change
    const [settings, setSettings] = useRefState(props.settings);
    useEffect(() => {
        setSettings(props.settings);
    }, [props.settings]);

    const editor = useRef<HTMLDivElement>(null);
    const view = useRef<EditorView | null>(null);

    const [firstLayout, setFirstLayout] = useState(true);

    const outputRef = useRef<PlaygroundRunner>(null);

    const [analysis, setAnalysis] = useRefState<AnalysisOutput | undefined>(undefined);
    const [output, setOutput] = useState<Output | undefined>();

    const [hoverPos, setHoverPos] = useRefState<[number, number] | undefined>(undefined);

    const hover = hoverTooltip(async (view, pos, side) => {
        if (outputRef.current!.isRunning()) {
            return null;
        }

        const { from, to } = syntaxTree(view.state).cursorAt(pos, side);

        // Disable hovering in beginner mode, except for diagnostics
        const hoverOutput =
            settings.current!.beginner ?? true ? null : await outputRef.current!.hover(from, to);

        if (!hoverOutput) {
            setHoverPos(undefined);
            return null;
        }

        setHoverPos([from, to]);

        return {
            pos: from,
            end: to,
            create: () => {
                const dom = document.createElement("div");
                ReactDOM.createRoot(dom).render(<Hover hover={hoverOutput} />);

                return {
                    dom,
                    destroy: () => setHoverPos(undefined),
                };
            },
        };
    });

    const [syntaxHighlighting, setSyntaxHighlighting] = useRefState<
        AnalysisOutputSyntaxHighlightingItem[]
    >([]);

    const highlightDecoration = (kind: string) =>
        Decoration.mark({
            attributes: {
                class: `token ${kind}`,
            },
        });

    const groupDecoration = (color: string) =>
        Decoration.mark({
            attributes: {
                class: color,
            },
        });

    const insertButtonDecoration = (
        from: number,
        to: number,
        head: number,
        hasSelection: boolean,
        leftMargin: number
    ) =>
        Decoration.widget({
            widget: new InsertButtonDecoration(from, to, hasSelection, leftMargin, () =>
                showInsertMenu(head)
            ),
            side: 1,
        });

    const placeholderDecoration = (placeholder: string, from: number, to: number) =>
        Decoration.replace({
            atomic: true,
            widget: new PlaceholderDecoration(placeholder, from, to, () => {
                view.current!.dispatch({ selection: { anchor: from, head: to } });
            }),
        });

    const assetDecoration = (
        asset: string,
        from: number,
        to: number,
        onChange: (asset: string) => void
    ) =>
        Decoration.replace({
            atomic: true,
            widget: new AssetDecoration(asset, from, to, onChange, () => {
                view.current!.dispatch({ selection: { anchor: from, head: to } });
            }),
        });

    const getHighlightDecorations = (view: EditorView) => {
        const tree = syntaxTree(view.state);

        const items = [...syntaxHighlighting.current!];
        const diagnostics = [...(analysis.current?.diagnostics ?? [])];

        const decorations: Range<Decoration>[] = [];
        const stack: string[] = [];
        tree.iterate({
            enter: (node) => {
                const { from, to } = node;

                // Decorations may not be empty
                if (from === to) {
                    return;
                }

                const nodeText = view.state.sliceDoc(from, to);

                // Decorations may not span multiple lines
                if (nodeText.includes("\n")) {
                    return;
                }

                switch (node.type.name) {
                    case "Placeholder":
                        // Remove the leading and trailing delimiters
                        const placeholder = nodeText.slice(2, nodeText.length - 2);

                        decorations.push(
                            placeholderDecoration(placeholder, from, to).range(from, to)
                        );

                        break;
                    case "Asset":
                        // Remove the leading and trailing backticks
                        const asset = nodeText.slice(1, nodeText.length - 1);

                        const onChangeAsset = (asset: string) => {
                            view.dispatch({
                                changes: {
                                    from: from + 1,
                                    to: to - 1,
                                    insert: asset,
                                },
                            });
                        };

                        decorations.push(
                            assetDecoration(asset, from, to, onChangeAsset).range(from, to)
                        );

                        break;
                    default:
                        break;
                }

                if ((settings.current.beginner ?? true) && node.node.firstChild == null) {
                    let color: string | undefined;
                    if (closingBrackets[nodeText]) {
                        color = bracketPairColors[stack.length % bracketPairColors.length];
                        stack.push(color);
                    } else if (Object.values(closingBrackets).includes(nodeText)) {
                        color = stack.pop()!;
                    } else {
                        color = stack[stack.length - 1];
                    }

                    if (color) {
                        decorations.push(groupDecoration(color).range(from, to));
                    }
                }

                if (hoverPos.current) {
                    const [start, end] = hoverPos.current;

                    if (start === from && end === to) {
                        decorations.push(highlightDecoration("hover").range(from, to));
                    }
                }

                for (const diagnostic of diagnostics) {
                    if (to > diagnostic.span.end) {
                        continue;
                    }

                    if (from >= diagnostic.span.start && to > from) {
                        decorations.push(
                            highlightDecoration(`diagnostic diagnostic-${diagnostic.level}`).range(
                                from,
                                to
                            )
                        );
                    }
                }

                if (items[0] && items[0].start === from && items[0].end === to && to > from) {
                    const { start, end, kind } = items.shift()!;
                    decorations.push(highlightDecoration(kind).range(start, end));
                }
            },
        });

        return Decoration.set(decorations, true);
    };

    const highlight = ViewPlugin.fromClass(
        class {
            public decorations: DecorationSet;

            constructor(view: EditorView) {
                this.decorations = getHighlightDecorations(view);
            }

            update(update: ViewUpdate) {
                this.decorations = getHighlightDecorations(update.view);
            }
        },
        {
            decorations: (v) => v.decorations,
            provide: (plugin) =>
                EditorView.atomicRanges.of((view) => {
                    const decorations = view.plugin(plugin)?.decorations;
                    if (!decorations) {
                        return Decoration.none;
                    }

                    return decorations.update({
                        filter: (_from, _to, decoration) => decoration.spec.atomic,
                    });
                }),
        }
    );

    const getInsertButtonDecorations = (view: EditorView) => {
        if (!view.hasFocus || view.state.selection.ranges.length !== 1) {
            return Decoration.none;
        }

        let pos: number | undefined;
        syntaxTree(view.state).iterate({
            enter: (node) => {
                if (node.node.firstChild != null) {
                    return true;
                }

                const { from, to } = node;

                // Decorations may not be empty
                if (from === to) {
                    return;
                }

                const nodeText = view.state.sliceDoc(from, to);

                // Decorations may not span multiple lines
                if (nodeText.includes("\n")) {
                    return;
                }

                if (
                    pos == null &&
                    view.state.selection.main.from >= from &&
                    view.state.selection.main.to <= to
                ) {
                    pos = node.to;
                    return false;
                }

                return false;
            },
        });

        pos = pos ?? view.state.selection.main.to;

        const head = view.state.doc.lineAt(view.state.selection.main.to).to;

        const leftMargin = activeInlineCompletion.current.trimEnd().length;

        return Decoration.set(
            insertButtonDecoration(
                head,
                head,
                pos,
                !view.state.selection.main.empty,
                leftMargin
            ).range(head, head)
        );
    };

    const insertButton = ViewPlugin.fromClass(
        class {
            public decorations: DecorationSet;

            constructor(view: EditorView) {
                this.decorations = getInsertButtonDecorations(view);
            }

            update(update: ViewUpdate) {
                this.decorations = getInsertButtonDecorations(update.view);
            }
        },
        {
            decorations: (v) => v.decorations,
        }
    );

    const [snippets, setSnippets] = useRefState<AnalysisOutputSnippets | null>(null);

    const canCollapse = props.code.length > 0 && output?.isEmpty;

    const [codeEditorContainerRef, { height: codeEditorContainerHeight }] = useMeasure();

    const animatedCodeEditorStyleDefaults = {
        immediate: firstLayout,
        onRest: () => setFirstLayout(false),
    };

    const animatedCodeEditorStyle = useSpring(
        canCollapse && props.collapse
            ? { ...animatedCodeEditorStyleDefaults, opacity: 0, height: 0 }
            : { ...animatedCodeEditorStyleDefaults, opacity: 1, height: codeEditorContainerHeight }
    );

    const [containsTemplates, setContainsTemplates] = useRefState(false);

    const onChange = (update: ViewUpdate) => {
        props.onChange(update.state.doc.toString());

        const tree = syntaxTree(update.view.state);

        let containsTemplates = false;
        tree.iterate({
            enter: (node) => {
                containsTemplates ||= node.type.name === "Placeholder";
                return !containsTemplates;
            },
        });

        setContainsTemplates(containsTemplates);
    };

    const handleFocusIn = (_e: FocusEvent, view: EditorView) => {
        props.onFocus({
            jumpToBeginning: () => {
                commands.cursorDocStart(view);
            },
            jumpToEnd: () => {
                commands.cursorDocEnd(view);
            },
            undo: () => {
                commands.undo(view);
            },
            redo: () => {
                commands.redo(view);
            },
            cut: () => {
                const { from, to } = view.state.selection.main;
                const text = view.state.sliceDoc(from, to);
                view.dispatch({ changes: { from, to, insert: "" } });
                return text;
            },
            copy: () => {
                const { from, to } = view.state.selection.main;
                return view.state.sliceDoc(from, to);
            },
            paste: (text) => {
                const { from, to } = view.state.selection.main;
                view.dispatch({ changes: { from, to, insert: text } });
                return text;
            },
            selectAll: () => {
                commands.selectAll(view);
            },
        });
    };

    const handleFocusOut = (e: FocusEvent) => {
        props.onBlur(e);
    };

    const onReset = useCallback(() => {
        // FIXME: Prevent flashing
        setSyntaxHighlighting([]);
        view.current!.dispatch({ effects: closeHoverTooltips });
    }, [setSyntaxHighlighting]);

    const onAnalyze = useCallback(
        (analysis: AnalysisOutput) => {
            setAnalysis(analysis);
            setSyntaxHighlighting(analysis.syntaxHighlighting);
            setSnippets(analysis.snippets);
            view.current!.dispatch();
        },
        [setSyntaxHighlighting, setSnippets]
    );

    const onError = useCallback((error: any) => {
        Sentry.captureException(error, (ctx) => {
            ctx.setContext("code-editor", { ...props });
            return ctx;
        });
    }, []);

    const [contextMenuAnchor, setContextMenuAnchor] = useState<HTMLElement>();

    const showInsertMenu = (pos: number) => {
        const caretPosition = view.current!.coordsAtPos(pos);

        if (!caretPosition) {
            console.error("attempt to show context menu without caret position");
            return;
        }

        const anchor = document.createElement("div");
        anchor.style.position = "absolute";
        anchor.style.top = `${caretPosition.top + 20}px`;
        anchor.style.left = `${caretPosition.left}px`;

        document.getElementById(containerID)!.appendChild(anchor);

        setContextMenuAnchor(anchor);
    };

    const hideContextMenu = () => {
        if (!contextMenuAnchor) return;

        contextMenuAnchor.remove();
        setContextMenuAnchor(undefined);
    };

    const buttonIconStyles = {
        fontSize: "14pt",
        width: 26,
        marginTop: "-0.125rem",
    };

    const insertSnippet = async (snippet: Snippet | SpecialSnippet, wrap: boolean) => {
        const code = props.code;

        if ("lineMode" in snippet && snippet.lineMode != null) {
            const line = view.current!.state.doc.lineAt(view.current!.state.selection.main.head);

            let pos: number;
            let newPos: number;
            switch (snippet.lineMode) {
                case "before": {
                    pos = line.from;
                    newPos = pos;
                    break;
                }
                case "after": {
                    pos = line.to;
                    newPos = pos + 1;
                    break;
                }
            }

            view.current!.dispatch({
                changes: {
                    from: pos,
                    to: pos,
                    insert: snippet.code,
                },
                selection: {
                    anchor: newPos,
                },
            });

            return;
        }

        const selection = Math.min(
            syntaxTree(view.current!.state).cursorAt(view.current!.state.selection.main.to, 1).to,
            view.current!.state.doc.lineAt(view.current!.state.selection.main.to).to
        );

        const beforeSelection = code.slice(0, selection);
        const padBefore = (beforeSelection[beforeSelection.length - 1] ?? " ").match(/\s/)
            ? ""
            : " ";

        const afterSelection = code.slice(selection);
        const padAfter = (afterSelection[0] ?? " ").match(/\s/) ? "" : " ";

        let newCode: string;
        if ("code" in snippet) {
            newCode = beforeSelection + padBefore + snippet.code + padAfter + afterSelection;
        } else {
            const { from, to } = view.current!.state.selection.main;

            const before = view.current!.state.sliceDoc(0, from);
            const after = view.current!.state.sliceDoc(to);
            const code = wrap ? view.current!.state.sliceDoc(from, to) : null;

            const expanded = await outputRef.current!.expandSnippet(snippet.id, code);
            if (expanded == null) {
                return;
            }

            newCode = wrap
                ? before + expanded + after
                : before + padBefore + expanded + padAfter + after;
        }

        formatCode(newCode);
    };

    const download = async () => {
        let html = await (await fetch("/playground/embed.html")).text();
        html = html.replaceAll("{{ORIGIN}}", window.location.origin);
        html = html.replace("{{CODE}}", props.code);
        html = html.replace("{{SETUP}}", props.setup ?? "");

        const blob = new Blob([html], { type: "text/html" });

        const url = URL.createObjectURL(blob);
        const a = document.createElement("a");
        document.body.appendChild(a);
        a.style.display = "none";
        a.href = url;
        a.download = "playground-export.html";
        a.click();
        URL.revokeObjectURL(url);
    };

    const themeConfig = useMemo(() => new Compartment(), []);

    const [collaborationMode, setCollaborationMode] = useRefState<"host" | "join" | undefined>(
        undefined
    );
    const [userId, setUserId] = useRefState<string | undefined>(undefined);
    const connectedUsers = useRef<string[]>([]);
    const collaborationOnHostReceive = useRef<((data: any) => void) | null>(null);
    const collaborationOnPeerReceive = useRef<((user: string, data: any) => void) | null>(null);
    const collaboration = useCollaboration({
        onHostReceive: (data) => collaborationOnHostReceive.current?.(data),
        onHostLeft: () => stopCollaborating(),
        onPeerJoined: (user) => {
            connectedUsers.current.push(user);
            sendCodeToPeer(user);
            sendCursorToPeer(user);
        },
        onPeerReceive: (user, data) => collaborationOnPeerReceive.current?.(user, data),
    });

    const startCollaborating = async (mode: "host" | "join") => {
        const { userId } = await collaboration.initialize();
        setCollaborationMode(mode);
        setUserId(userId);
        updateEditable();
        updateCollaborationStyles();
    };

    const joinedHostUserId = useRef<string | undefined>();

    const joinHost = async (hostUserId: string) => {
        await collaboration.connectToHost(hostUserId);
        joinedHostUserId.current = hostUserId;
        updateEditable();
        updateCollaborationStyles();
    };

    const stopCollaborating = async () => {
        await collaboration.disconnect();
        setCollaborationMode(undefined);
        setUserId(undefined);
        joinedHostUserId.current = undefined;
        cursors.current = {};
        updateEditable();
        updateCollaborationStyles();
    };

    const cursors = useRef<Record<string, RemoteCursor>>({});

    const isEditable = () =>
        Object.values(cursors.current).filter((cursor) => cursor != null).length === 0;

    const editableConfig = useMemo(() => new Compartment(), []);
    const updateEditable = () => {
        view.current?.dispatch({
            effects: editableConfig.reconfigure(EditorView.editable.of(isEditable())),
        });
    };

    const sendCursorToHost = async () => {
        try {
            const range = view.current!.hasFocus
                ? view.current!.state.selection.asSingle().ranges[0]
                : undefined;

            await collaboration.sendToHost({
                cursor: range
                    ? {
                          name: settings.current!.name || "Anonymous",
                          from: range.from,
                          to: range.to,
                          anchor: range.anchor,
                      }
                    : null,
            });
        } catch (error) {
            console.error(error);
        }
    };

    const sendCursorToPeer = async (user: string) => {
        try {
            const range = view.current!.hasFocus
                ? view.current!.state.selection.asSingle().ranges[0]
                : undefined;

            await collaboration.sendToPeer(user, {
                cursor: range
                    ? {
                          name: settings.current!.name || "Anonymous",
                          from: range.from,
                          to: range.to,
                          anchor: range.anchor,
                      }
                    : null,
            });
        } catch (error) {
            console.error(error);
        }
    };

    const sendCursorToAllPeers = async (options?: { except: string }) => {
        try {
            const range = view.current!.hasFocus
                ? view.current!.state.selection.asSingle().ranges[0]
                : undefined;

            await collaboration.sendToAllPeers(
                {
                    cursor: range
                        ? {
                              name: settings.current!.name || "Anonymous",
                              from: range.from,
                              to: range.to,
                              anchor: range.anchor,
                          }
                        : null,
                },
                options
            );
        } catch (error) {
            console.error(error);
        }
    };

    const sendCodeToHost = async () => {
        try {
            await collaboration.sendToHost({ code: view.current!.state.doc.toString() });
        } catch (error) {
            console.error(error);
        }
    };

    const sendCodeToPeer = async (user: string) => {
        try {
            await collaboration.sendToPeer(user, { code: view.current!.state.doc.toString() });
        } catch (error) {
            console.error(error);
        }
    };

    const sendCodeToAllPeers = async (options?: { except: string }) => {
        try {
            await collaboration.sendToAllPeers(
                { code: view.current!.state.doc.toString() },
                options
            );
        } catch (error) {
            console.error(error);
        }
    };

    const peerExtension = useMemo(
        () =>
            ViewPlugin.fromClass(
                class {
                    private sender: string | null | undefined;

                    constructor(private view: EditorView) {
                        collaborationOnHostReceive.current = (data) => {
                            this.sender = null;

                            if ("code" in data) {
                                this.view.dispatch({
                                    changes: {
                                        from: 0,
                                        to: this.view.state.doc.length,
                                        insert: data.code,
                                    },
                                });
                            }

                            if ("cursor" in data) {
                                cursors.current[joinedHostUserId.current!] = data.cursor;
                                updateEditable();
                                updateCollaborationStyles();
                                this.view.dispatch();
                            }
                        };

                        collaborationOnPeerReceive.current = (user, data) => {
                            this.sender = user;

                            if ("code" in data) {
                                this.view.dispatch({
                                    changes: {
                                        from: 0,
                                        to: this.view.state.doc.length,
                                        insert: data.code,
                                    },
                                });
                            }

                            if ("cursor" in data) {
                                cursors.current[user] = data.cursor;
                                updateEditable();
                                updateCollaborationStyles();
                                this.view.dispatch();
                            }
                        };
                    }

                    async update(update: ViewUpdate) {
                        const sender = this.sender;
                        this.sender = undefined;

                        if (!userId.current) return;

                        if (update.docChanged && isEditable()) {
                            if (sender === null) {
                                // The code was sent from the host; do nothing
                            } else if (sender === undefined) {
                                // The code was updated locally
                                switch (collaborationMode.current) {
                                    case "host":
                                        await sendCodeToAllPeers();
                                        break;
                                    case "join":
                                        await sendCodeToHost();
                                        break;
                                }
                            } else {
                                // The code was sent from a peer
                                await sendCodeToAllPeers({ except: sender });
                            }
                        }

                        if (update.focusChanged || update.selectionSet) {
                            if (!isEditable()) {
                                return;
                            }

                            if (sender === null) {
                                // The selection was sent from the host; do nothing
                            } else if (sender === undefined) {
                                // The selection was updated locally
                                switch (collaborationMode.current) {
                                    case "host":
                                        await sendCursorToAllPeers();
                                        break;
                                    case "join":
                                        await sendCursorToHost();
                                        break;
                                }
                            } else {
                                // The selection was sent from a peer
                                await sendCursorToAllPeers({ except: sender });
                            }

                            updateEditable();
                            updateCollaborationStyles();
                        }
                    }

                    async destroy() {
                        await collaboration.disconnect();
                    }
                }
            ),
        []
    );

    const activeInlineCompletion = useRef("");

    const inlineCompletions = useMemo(
        () =>
            inlineSuggestion({
                fetchFn: async (update) => {
                    let pos = 0;
                    activeInlineCompletion.current = await (async () => {
                        if (update.state.doc.length === 0 || outputRef.current!.isRunning()) {
                            return "";
                        }

                        const node = syntaxTree(update.state).cursorAt(
                            update.state.selection.main.to,
                            -1
                        );

                        if (
                            node.to !== update.state.selection.main.to ||
                            node.node.firstChild != null ||
                            !["Name", "Type"].includes(node.type.name)
                        ) {
                            return "";
                        }

                        const prefix = update.state.sliceDoc(node.from, node.to);
                        let completion = await outputRef.current!.completion(prefix);

                        if (!completion) {
                            return "";
                        }

                        completion = completion.slice(prefix.length);

                        if (!completion) {
                            return "";
                        }

                        pos = node.to;

                        return completion + " ";
                    })();

                    requestAnimationFrame(() => {
                        update.view.dispatch();
                    });

                    return activeInlineCompletion.current
                        ? { pos, text: activeInlineCompletion.current }
                        : null;
                },
            }),
        []
    );

    const autocompleteExtensions = useCallback(
        () =>
            settings.current!.autocomplete ?? false
                ? [Prec.high(keymap.of(closeBracketsKeymap)), closeBrackets(), inlineCompletions]
                : [],
        []
    );

    const autocompleteConfig = useMemo(() => new Compartment(), []);

    useEffect(() => {
        view.current?.dispatch({
            effects: autocompleteConfig.reconfigure(autocompleteExtensions()),
        });
    }, [settings.current!.autocomplete]);

    const focusExtension = ViewPlugin.fromClass(
        class {
            private fadeDecoration = Decoration.line({ class: "opacity-50" });
            private focusDecoration = Decoration.line({ class: "bg-blue-500 bg-opacity-10" });

            public decorations = Decoration.none;

            async update(update: ViewUpdate) {
                if (!update.focusChanged && !update.selectionSet) {
                    return;
                }

                if (!update.view.hasFocus) {
                    this.decorations = Decoration.none;
                    return;
                }

                // Only focus when the selection is empty
                if (update.state.selection.main.from !== update.state.selection.main.to) {
                    return;
                }

                const pos = update.state.selection.main.from;

                const decorations: Range<Decoration>[] = [];
                for (let n = 1; n <= update.state.doc.lines; n++) {
                    const line = update.state.doc.line(n);

                    const decoration =
                        pos < line.from || pos > line.to
                            ? this.fadeDecoration
                            : this.focusDecoration;

                    decorations.push(decoration.range(line.from, line.from));
                }

                this.decorations = Decoration.set(decorations);
            }
        },
        {
            decorations: (v) => v.decorations,
        }
    );

    const focusExtensions = useCallback(
        () => (settings.current!.focus ?? false ? [Prec.high(focusExtension)] : []),
        []
    );

    const focusConfig = useMemo(() => new Compartment(), []);

    useEffect(() => {
        view.current?.dispatch({
            effects: focusConfig.reconfigure(focusExtensions()),
        });
    }, [settings.current!.focus]);

    useEffect(() => {
        view.current = new EditorView({
            state: EditorState.create({
                doc: props.code,
                extensions: [
                    minimalSetup,
                    wippleLanguage,
                    EditorView.lineWrapping,
                    EditorView.baseTheme({
                        "&.cm-editor": {
                            fontSize: codeEditorFontSize,
                        },
                        "&.cm-editor.cm-focused": {
                            outline: "none",
                        },
                        ".cm-scroller": {
                            fontFamily: "'JetBrains Mono', monospace",
                            fontVariantLigatures: "none",
                        },
                        ".cm-content": {
                            padding: "1rem",
                        },
                        ".cm-line": {
                            padding: 0,
                        },
                        ".cm-placeholder": {
                            fontStyle: "italic",
                        },
                        ".cm-tooltip-hover": {
                            border: "none",
                            backgroundColor: "unset",
                        },
                    }),
                    themeConfig.of(prefersDarkMode ? githubDark : githubLight),
                    placeholder("Write your code here!"),
                    hover,
                    highlight,
                    insertButton,
                    keymap.of([...defaultKeymap, indentWithTab]),
                    peerExtension,
                    remoteCursorsTheme,
                    remoteCursors(() => cursors.current),
                    EditorView.updateListener.of(onChange),
                    EditorView.domEventHandlers({
                        focusin: handleFocusIn,
                        focusout: handleFocusOut,
                    }),
                    editableConfig.of(EditorView.editable.of(true)),
                    autocompleteConfig.of(autocompleteExtensions()),
                    focusConfig.of(focusExtensions()),
                    EditorState.allowMultipleSelections.of(false),
                ],
            }),
            parent: editor.current!,
        });

        return () => {
            view.current!.destroy();
        };
    }, []);

    useEffect(() => {
        view.current!.dispatch({
            effects: themeConfig.reconfigure([prefersDarkMode ? githubDark : githubLight]),
        });
    }, [prefersDarkMode]);

    useEffect(() => {
        view.current!.dispatch();
    }, [props.settings]);

    const getCollaborationStyles = () => {
        const firstCursor = Object.entries(cursors.current).find(
            ([_user, cursor]) => cursor != null
        );

        if (!firstCursor) {
            return undefined;
        }

        const [user, _cursor] = firstCursor;
        const color = userColor(user);

        return {
            borderColor: `${color}40`,
            "--tw-shadow-color": `${color}20`,
        };
    };

    const [collaborationStyles, setCollaborationStyles] = useState(getCollaborationStyles);
    const updateCollaborationStyles = () => setCollaborationStyles(getCollaborationStyles());

    const formatCode = async (code: string) => {
        const formatted = await outputRef.current!.format(code);
        if (formatted != null) {
            view.current!.dispatch({
                changes: {
                    from: 0,
                    to: view.current!.state.doc.length,
                    insert: formatted.trimEnd(),
                },
                selection: {
                    anchor: Math.min(
                        view.current!.state.selection.main.head,
                        formatted.trimEnd().length
                    ),
                },
            });
        }
    };

    return (
        <div id={containerID}>
            <div className="relative -mt-3.5">
                <div className="flex flex-row justify-end w-full pr-4 -mb-3.5">
                    <div
                        className="flex code-editor-outlined rounded-md shadow-lg shadow-gray-100 dark:shadow-gray-900 h-7 text-gray-500 text-opacity-50 z-10"
                        style={collaborationStyles}
                    >
                        {props.setup ? (
                            <Tooltip title="Documentation">
                                <a
                                    target="_blank"
                                    href={`/doc/${props.setup}.html`}
                                    className="code-editor-button -my-0.5 -ml-0.5 px-0.5"
                                >
                                    <SetupIcon setup={props.setup} size="medium" />
                                </a>
                            </Tooltip>
                        ) : null}

                        {collaborationMode.current ? (
                            <PopupState variant="popover">
                                {(popupState) => (
                                    <>
                                        <Tooltip title="Collaborate">
                                            <button
                                                {...bindTrigger(popupState)}
                                                className="code-editor-button -ml-0.5 px-2"
                                            >
                                                <PeopleAltRounded
                                                    fontSize="small"
                                                    sx={{ marginRight: "6px", marginTop: "-2px" }}
                                                />

                                                {collaborationMode.current === "host"
                                                    ? userId.current!
                                                    : joinedHostUserId.current!}
                                            </button>
                                        </Tooltip>

                                        <Menu {...bindMenu(popupState)}>
                                            <MenuItem
                                                onClick={() => {
                                                    popupState.close();

                                                    navigator.clipboard.writeText(userId.current!);
                                                }}
                                            >
                                                Copy Join Code
                                            </MenuItem>

                                            <MenuItem
                                                onClick={() => {
                                                    // This button doesn't actually do anything
                                                    // except take focus away from the code editor
                                                    // so that other people can edit
                                                    popupState.close();
                                                }}
                                            >
                                                Done Editing
                                            </MenuItem>

                                            <MenuItem
                                                onClick={() => {
                                                    popupState.close();

                                                    let message =
                                                        "Do you want to stop collaborating?";
                                                    if (collaborationMode.current === "host") {
                                                        message +=
                                                            " This will disconnect everyone from the session.";
                                                    }

                                                    const confirmed = confirm(message);

                                                    if (!confirmed) return;

                                                    stopCollaborating();
                                                }}
                                            >
                                                Stop Collaborating
                                            </MenuItem>
                                        </Menu>
                                    </>
                                )}
                            </PopupState>
                        ) : (
                            <PopupState variant="popover">
                                {(popupState) => (
                                    <>
                                        <Tooltip title="Collaborate">
                                            <button
                                                {...bindTrigger(popupState)}
                                                className="code-editor-button -ml-0.5"
                                            >
                                                <PeopleAltRounded
                                                    fontSize="small"
                                                    sx={buttonIconStyles}
                                                />
                                            </button>
                                        </Tooltip>

                                        <Menu {...bindMenu(popupState)}>
                                            <MenuItem
                                                onClick={async () => {
                                                    popupState.close();
                                                    await startCollaborating("host");
                                                }}
                                            >
                                                <PeopleAltRounded />
                                                Collaborate
                                            </MenuItem>

                                            <MenuItem
                                                onClick={async () => {
                                                    popupState.close();

                                                    if (props.code.length > 0) {
                                                        const confirmed = confirm(
                                                            "Joining someone else's coding session will clear your existing code. Are you sure you want to join?"
                                                        );
                                                        if (!confirmed) return;
                                                    }

                                                    const hostUserId = prompt(
                                                        "Please enter the ID of the session you want to join:"
                                                    );

                                                    if (!hostUserId) return;

                                                    await startCollaborating("join");
                                                    await joinHost(hostUserId);
                                                }}
                                            >
                                                <GroupAddRounded />
                                                Join
                                            </MenuItem>
                                        </Menu>
                                    </>
                                )}
                            </PopupState>
                        )}

                        <Tooltip title="Format">
                            <button
                                className="code-editor-button -mx-0.5"
                                disabled={props.code.length === 0}
                                onMouseDown={() => formatCode(props.code)}
                            >
                                <SubjectRounded sx={buttonIconStyles} />
                            </button>
                        </Tooltip>

                        <PopupState variant="popover">
                            {(popupState) => (
                                <>
                                    <Tooltip title="More">
                                        <button
                                            {...bindTrigger(popupState)}
                                            className="code-editor-button -mr-0.5"
                                        >
                                            <MoreHoriz sx={buttonIconStyles} />
                                        </button>
                                    </Tooltip>

                                    <Menu {...bindMenu(popupState)}>
                                        <MenuItem
                                            onClick={async () => {
                                                popupState.close();

                                                if (containsTemplates.current) {
                                                    return;
                                                }

                                                props.onChangeAutoRun(!props.autoRun);
                                            }}
                                        >
                                            {props.autoRun && !containsTemplates.current ? (
                                                <PauseRounded />
                                            ) : (
                                                <PlayArrowRounded />
                                            )}

                                            {containsTemplates.current
                                                ? "Running Paused"
                                                : props.autoRun
                                                ? "Pause Running"
                                                : "Run"}
                                        </MenuItem>

                                        <MenuItem
                                            disabled={!canCollapse}
                                            onClick={async () => {
                                                popupState.close();

                                                if (!canCollapse) {
                                                    return;
                                                }

                                                props.onChangeCollapse(!props.collapse);
                                            }}
                                        >
                                            {canCollapse && props.collapse ? (
                                                <FullScreenRounded />
                                            ) : (
                                                <FullScreenExitRounded />
                                            )}

                                            {canCollapse && props.collapse ? "Expand" : "Collapse"}
                                        </MenuItem>

                                        <MenuItem
                                            onClick={async () => {
                                                popupState.close();

                                                download();
                                            }}
                                        >
                                            <Download />
                                            Download Website
                                        </MenuItem>
                                    </Menu>
                                </>
                            )}
                        </PopupState>
                    </div>
                </div>

                <div
                    className="code-editor-outlined rounded-lg shadow-lg shadow-transparent"
                    style={collaborationStyles}
                >
                    <animated.div style={firstLayout ? undefined : animatedCodeEditorStyle}>
                        <div ref={codeEditorContainerRef}>
                            <div className="language-wipple" ref={editor} />
                        </div>
                    </animated.div>

                    <PlaygroundRunner
                        ref={outputRef}
                        id={props.id}
                        code={props.code}
                        outputClassName="bg-gray-50 dark:bg-gray-800"
                        beginner={props.settings.beginner ?? true}
                        lint={props.lint}
                        setup={props.setup}
                        autoRun={props.autoRun}
                        containsTemplates={() => containsTemplates.current!}
                        onReset={onReset}
                        onAnalyze={onAnalyze}
                        onChangeOutput={setOutput}
                        onApplyFix={async (fix) => {
                            view.current!.dispatch({
                                changes: {
                                    from: fix.start,
                                    to: fix.end,
                                    insert: fix.replacement,
                                },
                            });

                            await formatCode(view.current!.state.doc.toString());

                            setHoverPos(undefined);

                            view.current!.dispatch({ effects: closeHoverTooltips });
                        }}
                        onError={onError}
                    />
                </div>

                {snippets.current && (
                    <Menu
                        open={contextMenuAnchor != null}
                        anchorEl={contextMenuAnchor}
                        onClose={hideContextMenu}
                        style={{ maxHeight: 500 }}
                    >
                        <MenuList disablePadding>
                            {(() => {
                                if (!view.current || !snippets.current) {
                                    return null;
                                }

                                const hasSelection = !view.current.state.selection.main.empty;

                                const renderSnippetItem =
                                    (wrap: boolean) =>
                                    (snippet: Snippet | SpecialSnippet, index: number) =>
                                        (
                                            <MenuItem
                                                key={index}
                                                onClick={() => {
                                                    insertSnippet(snippet, wrap);
                                                    hideContextMenu();
                                                }}
                                                style={{ maxWidth: 400, whiteSpace: "normal" }}
                                            >
                                                <ListItemIcon>
                                                    {"element" in snippet ? (
                                                        <snippet.element />
                                                    ) : (
                                                        <CodeIcon />
                                                    )}
                                                </ListItemIcon>

                                                <ListItemText>
                                                    <Markdown>{snippet.name}</Markdown>
                                                </ListItemText>
                                            </MenuItem>
                                        );

                                if (hasSelection) {
                                    return snippets.current.wrapping.map(renderSnippetItem(true));
                                } else {
                                    return [
                                        ...builtinSnippets.map(renderSnippetItem(false)),
                                        ...snippets.current.nonwrapping.map(
                                            renderSnippetItem(false)
                                        ),
                                    ];
                                }
                            })()}
                        </MenuList>
                    </Menu>
                )}
            </div>
        </div>
    );
};

const Hover = (props: { hover: HoverOutput }) => (
    <div className="mt-2 p-2 overflow-clip bg-white dark:bg-gray-900 border-2 border-gray-100 dark:border-gray-700 rounded-lg text-black dark:text-white">
        {props.hover.code ? (
            <div className="pointer-events-none">
                <SimpleCodeEditor
                    className="code-editor dark:caret-white"
                    textareaClassName="outline-0"
                    preClassName="language-wipple"
                    style={{
                        fontFamily: "'JetBrains Mono', monospace",
                        fontVariantLigatures: "none",
                        wordWrap: "break-word",
                    }}
                    value={props.hover.code}
                    highlight={(code) => prism.highlight(code, prism.languages.wipple, "wipple")}
                    onValueChange={() => {}}
                    contentEditable={false}
                />

                {props.hover.help ? <Markdown>{props.hover.help}</Markdown> : null}
            </div>
        ) : null}

        {props.hover.url ? (
            <div className="mt-1.5">
                <a
                    href={props.hover.url}
                    target="_blank"
                    className="px-1.5 py-0.5 rounded-md bg-blue-500 text-white"
                >
                    Documentation
                </a>
            </div>
        ) : null}
    </div>
);

class InsertButtonDecoration extends WidgetType {
    constructor(
        private from: number,
        private to: number,
        private hasSelection: boolean,
        private leftMargin: number,
        private onClick: () => void
    ) {
        super();
    }

    eq(widget: this): boolean {
        return (
            this.from === widget.from &&
            this.to === widget.to &&
            this.hasSelection === widget.hasSelection &&
            this.leftMargin === widget.leftMargin
        );
    }

    toDOM() {
        const container = document.createElement("span");
        ReactDOM.createRoot(container).render(
            <InsertButton
                hasSelection={this.hasSelection}
                leftMargin={this.leftMargin}
                onClick={this.onClick}
            />
        );

        return container;
    }
}

const InsertButton = (props: {
    hasSelection: boolean;
    leftMargin: number;
    onClick: () => void;
}) => {
    const [showButton, setShowButton] = useState(false);

    useEffect(() => {
        const showButton = setTimeout(() => setShowButton(true), 500);

        return () => {
            clearTimeout(showButton);
        };
    }, []);

    return (
        <div className="inline-block w-0 relative">
            <div
                className={`absolute ${
                    showButton ? "opacity-100" : "opacity-0"
                } transition-opacity`}
                style={{ fontSize: codeEditorFontSize, paddingLeft: `${props.leftMargin}ch` }}
            >
                <Tooltip
                    title="Insert"
                    disableHoverListener={!showButton}
                    hidden={!showButton}
                    className="ml-1"
                >
                    <div
                        className="flex items-center justify-center bg-blue-500 ml-1 mt-[-12pt] w-5 h-5 rounded-md text-white cursor-pointer"
                        onClick={props.onClick}
                    >
                        {props.hasSelection ? <CodeIcon /> : <AddRoundedIcon />}
                    </div>
                </Tooltip>
            </div>
        </div>
    );
};

class PlaceholderDecoration extends WidgetType {
    constructor(
        private placeholder: string,
        private from: number,
        private to: number,
        private onClick: () => void
    ) {
        super();
    }

    eq(widget: this): boolean {
        return (
            this.placeholder === widget.placeholder &&
            this.from === widget.from &&
            this.to === widget.to
        );
    }

    toDOM() {
        const container = document.createElement("span");
        ReactDOM.createRoot(container).render(
            <Placeholder placeholder={this.placeholder} onClick={this.onClick} />
        );

        return container;
    }
}

const Placeholder = (props: { placeholder: string; onClick: () => void }) => {
    return (
        <span
            className="inline-block bg-blue-500 bg-opacity-80 px-2 border-2 border-gray-100 dark:border-gray-700 rounded-lg text-white ui-font"
            onClick={props.onClick}
        >
            {props.placeholder}
        </span>
    );
};

class AssetDecoration extends WidgetType {
    constructor(
        private asset: string,
        private from: number,
        private to: number,
        private onChange: (asset: string) => void,
        private onClick: () => void
    ) {
        super();
    }

    eq(widget: this): boolean {
        return this.asset === widget.asset && this.from === widget.from && this.to === widget.to;
    }

    toDOM() {
        const container = document.createElement("span");
        ReactDOM.createRoot(container).render(
            <Asset asset={this.asset} onChange={this.onChange} onClick={this.onClick} />
        );
        return container;
    }
}

const Asset = (props: {
    asset: string;
    disabled?: boolean;
    onChange?: (asset: string) => void;
    onClick?: () => void;
}) => {
    const content = (() => {
        if (CSS.supports("color", props.asset)) {
            return <ColorAsset color={props.asset} onChangeColor={props.onChange} />;
        }

        if (/^[A-Ga-g][#b]?\d+$/.test(props.asset)) {
            return <NoteAsset note={props.asset} onChangeNote={props.onChange} />;
        }

        if (emojiRegex().test(props.asset) && graphemeSplit(props.asset).length === 1) {
            return <EmojiAsset emoji={props.asset} onChangeEmoji={props.onChange} />;
        }

        return <ErrorAsset />;
    })();

    return (
        <div
            className="inline-block w-4 h-4"
            style={{ pointerEvents: props.disabled ? "none" : undefined }}
            onClick={props.onClick}
        >
            {content}
        </div>
    );
};

const AssetContainer = (props: { error?: boolean; children: JSX.Element }) => (
    <div
        className={`flex w-full h-full border-2 ${
            props.error ? "border-red-500" : "border-gray-100 dark:border-gray-700"
        } rounded-[5px] overflow-clip`}
    >
        {props.children}
    </div>
);

const ColorAsset = (props: { color: string; onChangeColor?: (color: string) => void }) => (
    <AssetContainer>
        <PopupState variant="popover">
            {(popupState) => (
                <>
                    <div className="flex w-full h-full" style={{ backgroundColor: props.color }}>
                        <div
                            style={{
                                pointerEvents: props.onChangeColor == null ? "none" : undefined,
                            }}
                            className="w-full h-full"
                            {...bindToggle(popupState)}
                        />
                    </div>

                    <Popover
                        {...bindPopover(popupState)}
                        anchorOrigin={{ horizontal: "left", vertical: "bottom" }}
                    >
                        <SwatchesPicker
                            color={props.color}
                            onChangeComplete={(color) => {
                                props.onChangeColor?.(color.hex);
                                popupState.close();
                            }}
                        />
                    </Popover>
                </>
            )}
        </PopupState>
    </AssetContainer>
);

const NoteAsset = (props: { note: string; onChangeNote?: (note: string) => void }) => (
    <AssetContainer>
        <PopupState variant="popover">
            {(popupState) => (
                <>
                    <div className="flex w-full h-full" style={{ backgroundColor: "white" }}>
                        <div
                            style={{
                                pointerEvents: props.onChangeNote == null ? "none" : undefined,
                            }}
                            className="relative w-full h-full"
                            {...bindToggle(popupState)}
                        >
                            <Note>{props.note}</Note>
                        </div>
                    </div>

                    <Popover
                        {...bindPopover(popupState)}
                        anchorOrigin={{ horizontal: "left", vertical: "bottom" }}
                    >
                        <div className="max-w-[100vw] overflow-x-scroll">
                            <Piano
                                noteRange={{ first: 21, last: 108 }}
                                width={800}
                                playNote={() => {}}
                                stopNote={(midi: number) => {
                                    popupState.close();
                                    const note = tonal.Note.fromMidi(midi);
                                    props.onChangeNote?.(note);
                                }}
                                renderNoteLabel={({ midiNumber, isAccidental }: any) => {
                                    if (isAccidental) return null;

                                    const note = tonal.Note.fromMidi(midiNumber);

                                    return (
                                        <p className="w-full text-[6pt] text-black text-center">
                                            {note}
                                        </p>
                                    );
                                }}
                            />
                        </div>
                    </Popover>
                </>
            )}
        </PopupState>
    </AssetContainer>
);

const EmojiAsset = (props: { emoji: string; onChangeEmoji?: (emoji: string) => void }) => (
    <AssetContainer>
        <PopupState variant="popover">
            {(popupState) => (
                <>
                    <div className="flex w-full h-full">
                        <div
                            style={{
                                pointerEvents: props.onChangeEmoji == null ? "none" : undefined,
                            }}
                            className="relative w-full h-full"
                            {...bindToggle(popupState)}
                        >
                            <p
                                style={{
                                    position: "absolute",
                                    top: "-0.1em",
                                    left: "-0.1em",
                                    fontSize: "8pt",
                                    fontFamily: "Segoe UI Emoji",
                                }}
                            >
                                {props.emoji}
                            </p>
                        </div>
                    </div>

                    <Popover
                        {...bindPopover(popupState)}
                        anchorOrigin={{ horizontal: "left", vertical: "bottom" }}
                    >
                        <EmojiPicker
                            data={emojiData}
                            onEmojiSelect={(emoji: any) => props.onChangeEmoji?.(emoji.native)}
                        />
                    </Popover>
                </>
            )}
        </PopupState>
    </AssetContainer>
);

const ErrorAsset = () => (
    <AssetContainer error>
        <div
            className="w-full h-full"
            onClick={() => {
                alert("The asset failed to load. Make sure you've provided the correct name.");
            }}
        ></div>
    </AssetContainer>
);

const wippleLanguage = new LanguageSupport(
    LRLanguage.define({
        name: "Wipple",
        parser: parser.configure({
            props: [
                styleTags({
                    Comment: t.comment,
                    Placeholder: t.name,
                    QuoteName: t.name,
                    RepeatName: t.name,
                    Text: t.string,
                    Number: t.number,
                    Asset: t.string,
                    Keyword: t.keyword,
                    Operator: t.operator,
                    Type: t.typeName,
                    Name: t.name,
                }),
            ],
        }),
        languageData: {
            commentTokens: { line: "--" },
            wordChars: "-!?",
            closeBrackets: {
                brackets: ["(", "[", "{", '"'],
            },
        },
    })
);

const styles = [
    {
        tag: t.comment,
        class: "token comment",
    },
    {
        tag: t.string,
        class: "token text",
    },
    {
        tag: t.number,
        class: "token number",
    },
    {
        tag: t.keyword,
        class: "token keyword",
    },
    {
        tag: t.operator,
        class: "token operator",
    },
    {
        tag: t.typeName,
        class: "token type",
    },
    {
        tag: t.name,
        class: "token name",
    },
];

const githubLight = githubLightInit({
    settings: {
        background: "transparent",
        foreground: "#24292e",
    },
    styles,
});

const githubDark = githubDarkInit({
    settings: {
        background: "transparent",
        foreground: "#e1e4e8",
    },
    styles,
});

const closingBrackets: Record<string, string> = {
    "(": ")",
    "{": "}",
    "[": "]",
};

const bracketPairColors: string[] = [
    "bg-blue-500 bg-opacity-20",
    "bg-red-500 bg-opacity-20",
    "bg-green-500 bg-opacity-20",
    "bg-yellow-500 bg-opacity-20",
];

const builtinSnippets: SpecialSnippet[] = [
    {
        element: () => <KeyboardReturnIcon />,
        name: "Insert line before",
        code: "\n",
        lineMode: "before",
    },
    {
        element: () => <KeyboardReturnIcon />,
        name: "Insert line after",
        code: "\n",
        lineMode: "after",
    },
    {
        element: () => <Asset asset="#007aff" disabled />,
        name: "Insert color",
        code: "`#007aff`",
        lineMode: null,
    },
    {
        element: () => <Asset asset="😀" disabled />,
        name: "Insert emoji",
        code: "`😀`",
        lineMode: null,
    },
    {
        element: () => <Asset asset="C4" disabled />,
        name: "Insert note",
        code: "`C4`",
        lineMode: null,
    },
];
