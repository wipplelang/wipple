import ReactDOM from "react-dom/client";
import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import SimpleCodeEditor from "./react-simple-code-editor";
import * as prism from "prismjs";
import Divider from "@mui/material/Divider";
import ListItemText from "@mui/material/ListItemText";
import ListSubheader from "@mui/material/ListSubheader";
import Menu from "@mui/material/Menu";
import MenuItem from "@mui/material/MenuItem";
import MenuList from "@mui/material/MenuList";
import TextField from "@mui/material/TextField";
import Tooltip from "@mui/material/Tooltip";
import useMediaQuery from "@mui/material/useMediaQuery";
import { Globals as SpringGlobals, useSpring, animated } from "react-spring";
import useMeasure from "react-use-measure";
import {
    AnalysisOutputDiagnostic,
    AnalysisOutputSyntaxHighlightingItem,
    HoverOutput,
    AnalysisOutputCompletions,
    Completion,
    AnalysisConsoleDiagnosticFix,
    useRefState,
    Markdown,
    PlaygroundRunner,
    AnalysisOutput,
    Output,
} from "shared";
import AddRounded from "@mui/icons-material/AddRounded";
import SubjectRounded from "@mui/icons-material/SubjectRounded";
import PlayArrowRounded from "@mui/icons-material/PlayArrowRounded";
import PauseRounded from "@mui/icons-material/PauseRounded";
import FullScreenRounded from "@mui/icons-material/FullscreenRounded";
import FullScreenExitRounded from "@mui/icons-material/FullscreenExitRounded";
import MoreHoriz from "@mui/icons-material/MoreHoriz";
import Download from "@mui/icons-material/Download";
import * as Sentry from "@sentry/react";
import PopupState, { bindMenu, bindTrigger } from "material-ui-popup-state";
import { minimalSetup } from "codemirror";
import { EditorView, placeholder, Decoration, ViewPlugin, DecorationSet } from "@codemirror/view";
import { Compartment, EditorState, Range } from "@codemirror/state";
import { syntaxTree } from "@codemirror/language";
import { defaultKeymap, indentWithTab } from "@codemirror/commands";
import { keymap, hoverTooltip, ViewUpdate, closeHoverTooltips } from "@codemirror/view";
import { githubLightInit, githubDarkInit } from "@uiw/codemirror-theme-github";
import { styleTags, tags as t } from "@lezer/highlight";
import { LRLanguage, LanguageSupport } from "@codemirror/language";
import { parser } from "../languages/wipple.grammar";
import { Settings } from "../App";

export interface CodeEditorProps {
    id: string;
    code: string;
    autoRun: boolean;
    onChangeAutoRun: (autoRun: boolean) => void;
    lint: boolean;
    collapse: boolean;
    onChangeCollapse: (collapse: boolean) => void;
    settings: Settings;
    autoFocus: boolean;
    onChange: (code: string) => void;
}

interface Hover {
    output: HoverOutput | null;
    diagnostic:
        | [AnalysisOutputDiagnostic, boolean, AnalysisOutputDiagnostic["notes"][number]]
        | undefined;
}

export const CodeEditor = (props: CodeEditorProps) => {
    const containerID = `code-editor-container-${props.id}`;

    const prefersDarkMode = useMediaQuery("(prefers-color-scheme: dark)");

    const prefersReducedMotion = useMediaQuery("(prefers-reduced-motion)");
    useEffect(() => {
        SpringGlobals.assign({
            skipAnimation: prefersReducedMotion,
        });
    }, [prefersReducedMotion]);

    const editor = useRef<HTMLDivElement>(null);
    const view = useRef<EditorView | null>(null);

    const [firstLayout, setFirstLayout] = useState(true);

    const outputRef = useRef<PlaygroundRunner>(null);

    const [analysis, setAnalysis] = useRefState<AnalysisOutput | undefined>(undefined);
    const [output, setOutput] = useState<Output | undefined>();

    const [hoverPos, setHoverPos] = useRefState<[number, number] | undefined>(undefined);

    const hover = hoverTooltip(async (view, pos, side) => {
        const { from, to, text } = view.state.doc.lineAt(pos);
        let start = pos,
            end = pos;
        while (start > from && /[^\s]/.test(text[start - from - 1])) start--;
        while (end < to && /[^\s]/.test(text[end - from])) end++;
        if ((start == pos && side < 0) || (end == pos && side > 0)) {
            setHoverPos(undefined);
            return null;
        }

        let hoverDiagnostic: Hover["diagnostic"];
        outer: for (const diagnostic of analysis.current?.diagnostics ?? []) {
            for (let noteIndex = 0; noteIndex < diagnostic.notes.length; noteIndex++) {
                const note = diagnostic.notes[noteIndex];

                if (note.span.start >= start && note.span.end <= end) {
                    hoverDiagnostic = [diagnostic, noteIndex === 0, note];
                    break outer;
                }
            }
        }

        const hoverOutput = await outputRef.current!.hover(start, end);

        if (!hoverDiagnostic && !hoverOutput) {
            setHoverPos(undefined);
            return null;
        }

        setHoverPos([start, end]);

        return {
            pos: start,
            end,
            create: () => {
                const dom = document.createElement("div");
                ReactDOM.createRoot(dom).render(
                    <Hover
                        hover={{ diagnostic: hoverDiagnostic, output: hoverOutput }}
                        onApplyFix={(fix) => {
                            view.dispatch({
                                changes: {
                                    from: fix.start,
                                    to: fix.end,
                                    insert: fix.replacement,
                                },
                            });

                            setHoverPos(undefined);

                            view.dispatch({ effects: closeHoverTooltips });
                        }}
                    />
                );

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

    const decoration = (kind: string) =>
        Decoration.mark({
            attributes: {
                class: `token ${kind}`,
            },
        });

    const getDecorations = (view: EditorView) => {
        const tree = syntaxTree(view.state);

        const items = [...syntaxHighlighting.current!];
        const diagnostics = [...(analysis.current?.diagnostics ?? [])];

        const decorations: Range<Decoration>[] = [];
        tree.iterate({
            enter: (node) => {
                if (hoverPos.current) {
                    const [start, end] = hoverPos.current;

                    if (start === node.from && end === node.to) {
                        decorations.push(decoration("hover").range(node.from, node.to));
                    }
                }

                for (const diagnostic of diagnostics) {
                    const notes = [...diagnostic.notes];
                    const primaryNote = notes.shift();
                    if (!primaryNote) {
                        continue;
                    }

                    if (node.to > primaryNote.span.end) {
                        continue;
                    }

                    if (node.from >= primaryNote.span.start && node.to > node.from) {
                        decorations.push(
                            decoration(`diagnostic diagnostic-${diagnostic.level}`).range(
                                node.from,
                                node.to
                            )
                        );
                    }
                }

                if (
                    items[0] &&
                    items[0].start === node.from &&
                    items[0].end === node.to &&
                    node.to > node.from
                ) {
                    const { start, end, kind } = items.shift()!;
                    decorations.push(decoration(kind).range(start, end));
                }
            },
        });

        return Decoration.set(decorations);
    };

    const highlight = ViewPlugin.fromClass(
        class {
            public decorations: DecorationSet;

            constructor(view: EditorView) {
                this.decorations = getDecorations(view);
            }

            update(update: ViewUpdate) {
                this.decorations = getDecorations(update.view);
            }
        },
        { decorations: (v) => v.decorations }
    );

    const [completions, setCompletions] = useState<AnalysisOutputCompletions>();

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

    const onReset = useCallback(() => {
        // FIXME: Prevent flashing
        setSyntaxHighlighting([]);
        view.current!.dispatch({ effects: closeHoverTooltips });
    }, [setSyntaxHighlighting]);

    const onAnalyze = useCallback(
        (analysis: AnalysisOutput) => {
            setAnalysis(analysis);
            setSyntaxHighlighting(analysis.syntaxHighlighting);
            setCompletions(analysis.completions);
            view.current!.dispatch();
        },
        [setSyntaxHighlighting, setCompletions]
    );

    const onError = useCallback((error: any) => {
        Sentry.captureException(error, (ctx) => {
            ctx.setContext("code-editor", { ...props });
            return ctx;
        });
    }, []);

    const [contextMenuAnchor, setContextMenuAnchor] = useState<HTMLElement>();
    const [contextMenuSearch, setContextMenuSearch] = useState("");

    const showContextMenu = () => {
        const caretPosition = view.current!.coordsAtPos(view.current!.state.selection.main.from);

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
        setContextMenuSearch("");
    };

    const buttonIconStyles = {
        fontSize: "14pt",
        width: 26,
        marginTop: "-0.125rem",
    };

    const insertCompletion = (completion: Completion) => {
        const code = props.code;

        const selection = view.current!.state.selection.main;

        const before = code.slice(0, selection.to);
        const padBefore = (before[before.length - 1] ?? " ").match(/\s/) ? "" : " ";

        const after = code.slice(selection.from);
        const padAfter = (after[0] ?? " ").match(/\s/) ? "" : " ";

        const text = padBefore + completion.template + padAfter;

        view.current!.dispatch({
            changes: {
                from: selection.from,
                to: selection.to,
                insert: text,
            },
            selection: {
                anchor: selection.to + text.length,
            },
        });

        props.onChange(before + padBefore + completion.template + padAfter + after);
    };

    const download = async () => {
        let html = await (await fetch("/playground/embed.html")).text();
        html = html.replaceAll("{{ORIGIN}}", window.location.origin);
        html = html.replace("{{CODE}}", props.code);

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

    useEffect(() => {
        view.current = new EditorView({
            state: EditorState.create({
                doc: props.code,
                extensions: [
                    minimalSetup,
                    wippleLanguage,
                    keymap.of([...defaultKeymap, indentWithTab]),
                    EditorView.lineWrapping,
                    EditorView.baseTheme({
                        "&.cm-editor": {
                            fontSize: "16px",
                        },
                        "&.cm-editor.cm-focused": {
                            outline: "none",
                        },
                        ".cm-scroller": {
                            fontFamily: "'JetBrains Mono', monospace",
                            fontVariantLigatures: "none",
                        },
                        ".cm-content": {
                            padding: 0,
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
                    EditorView.updateListener.of(onChange),
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

    return (
        <div id={containerID}>
            <div className="relative -mt-3.5">
                <div className="flex flex-row justify-end w-full pr-4 -mb-3.5">
                    <div className="code-editor-outlined rounded-md shadow-lg shadow-gray-100 dark:shadow-gray-900 h-7 text-gray-500 text-opacity-50 z-10">
                        <Tooltip title="Insert">
                            <button
                                className="code-editor-button -ml-0.5"
                                onMouseDown={(e) => {
                                    e.preventDefault();
                                    showContextMenu();
                                }}
                            >
                                <AddRounded sx={buttonIconStyles} />
                            </button>
                        </Tooltip>

                        <Tooltip title="Format">
                            <button
                                className="code-editor-button -mx-0.5"
                                disabled={props.code.length === 0}
                                onMouseDown={async (e) => {
                                    const formatted = await outputRef.current!.format();
                                    if (formatted != null) {
                                        view.current!.dispatch({
                                            changes: {
                                                from: 0,
                                                to: view.current!.state.doc.length,
                                                insert: formatted.trimEnd(),
                                            },
                                        });

                                        props.onChange(formatted.trimEnd());
                                    }
                                }}
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

                <div className="code-editor-outlined rounded-lg">
                    <animated.div style={firstLayout ? undefined : animatedCodeEditorStyle}>
                        <div ref={codeEditorContainerRef} className="p-4">
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
                        autoRun={props.autoRun}
                        containsTemplates={() => containsTemplates.current!}
                        onReset={onReset}
                        onAnalyze={onAnalyze}
                        onChangeOutput={setOutput}
                        onError={onError}
                    />
                </div>

                {completions && (
                    <Menu
                        open={contextMenuAnchor != null}
                        anchorEl={contextMenuAnchor}
                        onClose={hideContextMenu}
                        style={{ maxHeight: 500 }}
                    >
                        <TextField
                            inputMode="search"
                            value={contextMenuSearch}
                            onChange={(e) => setContextMenuSearch(e.target.value)}
                            placeholder="Search"
                            fullWidth
                        />

                        <MenuList disablePadding>
                            {(() => {
                                const includeCompletion = (completion: Completion) =>
                                    !contextMenuSearch ||
                                    contextMenuSearch.includes(completion.name) ||
                                    completion.name.includes(contextMenuSearch);

                                const renderCompletionItem = (
                                    completion: Completion,
                                    index: number
                                ) =>
                                    completion.help ? (
                                        <MenuItem
                                            key={index}
                                            onClick={() => {
                                                insertCompletion(completion);
                                                hideContextMenu();
                                            }}
                                            style={{ maxWidth: 400, whiteSpace: "normal" }}
                                        >
                                            <ListItemText>
                                                <pre className="language-wipple">
                                                    <span className={`token ${completion.kind}`}>
                                                        {completion.name}
                                                    </span>
                                                </pre>

                                                <Markdown>{completion.help}</Markdown>
                                            </ListItemText>
                                        </MenuItem>
                                    ) : null;

                                const languageSection = completions.language
                                    .filter(includeCompletion)
                                    .map(renderCompletionItem);

                                const variablesSection = completions.variables
                                    .filter(includeCompletion)
                                    .map(renderCompletionItem);

                                const groupedConstantsSection = completions.groupedConstants.map(
                                    ([group, completions], index) => {
                                        const filtered = completions.filter(includeCompletion);

                                        return filtered.length ? (
                                            <div key={index}>
                                                <Divider />
                                                <ListSubheader>{group}</ListSubheader>
                                                <Divider />
                                                {...filtered.map(renderCompletionItem)}
                                            </div>
                                        ) : null;
                                    }
                                );

                                const ungroupedConstantsSection = completions.ungroupedConstants
                                    .filter(includeCompletion)
                                    .map(renderCompletionItem);

                                return [
                                    languageSection,
                                    languageSection.length ? (
                                        <Divider key="languageDivider" />
                                    ) : null,
                                    variablesSection,
                                    variablesSection.length ? (
                                        <Divider key="variablesDivider" />
                                    ) : null,
                                    groupedConstantsSection,
                                    ungroupedConstantsSection.length ? (
                                        <Divider key="ungroupedConstantsDivider" />
                                    ) : null,
                                    ungroupedConstantsSection,
                                ];
                            })()}
                        </MenuList>
                    </Menu>
                )}
            </div>
        </div>
    );
};

const Hover = (props: {
    hover: Hover;
    onApplyFix: (fix: AnalysisConsoleDiagnosticFix) => void;
}) => {
    return (
        <div className="mt-2 p-2 overflow-clip bg-white dark:bg-gray-900 border-2 border-gray-100 dark:border-gray-700 rounded-lg text-black dark:text-white">
            {props.hover.diagnostic ? (
                <div className="flex flex-col">
                    <div
                        className={`font-bold ${
                            props.hover.diagnostic[0].level === "error"
                                ? "text-red-600 dark:text-red-500"
                                : "text-yellow-600 dark:text-yellow-500"
                        }`}
                    >
                        <Markdown>
                            {`${props.hover.diagnostic[0].level}: ${props.hover.diagnostic[0].message}`}
                        </Markdown>
                    </div>

                    <div className="flex flex-col">
                        {props.hover.diagnostic[2].messages.map((message, messageIndex) => (
                            <div
                                key={messageIndex}
                                className={
                                    messageIndex === 0 && props.hover.diagnostic![1]
                                        ? props.hover.diagnostic![0].level === "error"
                                            ? "text-red-600 dark:text-red-500"
                                            : "text-yellow-600 dark:text-yellow-500"
                                        : "opacity-75"
                                }
                            >
                                <Markdown>{message}</Markdown>
                            </div>
                        ))}
                    </div>

                    {props.hover.diagnostic[0].fix && (
                        <div className="flex">
                            <button
                                className="mt-1.5 px-1.5 py-0.5 rounded-md bg-blue-500 text-white"
                                onClick={() => {
                                    props.onApplyFix(props.hover.diagnostic![0].fix!);
                                }}
                            >
                                <Markdown>{props.hover.diagnostic[0].fix.description}</Markdown>
                            </button>
                        </div>
                    )}

                    {props.hover.output && (
                        <div className="h-0.5 my-2 bg-gray-100 dark:bg-gray-700"></div>
                    )}
                </div>
            ) : null}

            {props.hover.output?.code ? (
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
                        value={props.hover.output.code}
                        highlight={(code) =>
                            prism.highlight(code, prism.languages.wipple, "wipple")
                        }
                        onValueChange={() => {}}
                        contentEditable={false}
                    />

                    {props.hover.output.help ? (
                        <Markdown>{props.hover.output.help}</Markdown>
                    ) : null}
                </div>
            ) : null}

            {props.hover.output?.url ? (
                <div className="mt-1.5">
                    <a
                        href={props.hover.output.url}
                        target="_blank"
                        className="px-1.5 py-0.5 rounded-md bg-blue-500 text-white"
                    >
                        Documentation
                    </a>
                </div>
            ) : null}
        </div>
    );
};

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
        },
    })
);

const githubLight = githubLightInit({
    settings: {
        background: "transparent",
        foreground: "#24292e",
    },
    styles: [
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
    ],
});

const githubDark = githubDarkInit({
    settings: {
        background: "transparent",
        foreground: "#e1e4e8",
    },
    styles: [
        {
            tag: t.comment,
            class: "wipple-token-comment-dark",
        },
        {
            tag: t.string,
            class: "wipple-token-text-dark",
        },
        {
            tag: t.number,
            class: "wipple-token-number-dark",
        },
        {
            tag: t.keyword,
            class: "wipple-token-keyword-dark",
        },
        {
            tag: t.operator,
            class: "wipple-token-operator-dark",
        },
        {
            tag: t.typeName,
            class: "wipple-token-type-dark",
        },
        {
            tag: t.name,
            class: "wipple-token-name-dark",
        },
    ],
});
