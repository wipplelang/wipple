// Adapted from https://github.com/yjs/y-codemirror.next/blob/main/src/y-remote-selections.js

import * as cmView from "@codemirror/view";
import * as cmState from "@codemirror/state";
import * as dom from "lib0/dom";
import * as pair from "lib0/pair";
import ColorHash from "color-hash";

const colorHash = new ColorHash({ lightness: 0.5, saturation: 0.9 });
export const userColor = (user: string) => colorHash.hex(user);

export interface RemoteCursor {
    peer: string;
    name: string;
    from: number;
    to: number;
    anchor: number;
}

export const remoteCursorsTheme = cmView.EditorView.baseTheme({
    ".cm-scroller": {
        overflow: "visible",
    },
    ".cm-ySelection": {},
    ".cm-yLineSelection": {
        padding: 0,
        margin: "0px 2px 0px 4px",
    },
    ".cm-ySelectionCaret": {
        position: "relative",
        borderLeft: "1px solid black",
        borderRight: "1px solid black",
        marginLeft: "-1px",
        marginRight: "-1px",
        boxSizing: "border-box",
        display: "inline",
    },
    ".cm-ySelectionCaretDot": {
        borderRadius: "50%",
        position: "absolute",
        width: ".4em",
        height: ".4em",
        top: "-.2em",
        left: "-.2em",
        backgroundColor: "inherit",
        transition: "transform .3s ease-in-out",
        boxSizing: "border-box",
    },
    ".cm-ySelectionCaret:hover > .cm-ySelectionCaretDot": {
        transformOrigin: "bottom center",
        transform: "scale(0)",
    },
    ".cm-ySelectionInfo": {
        position: "absolute",
        top: "-1.05em",
        left: "-1px",
        fontSize: ".75em",
        fontFamily: "Inter, sans-serif",
        fontStyle: "normal",
        fontWeight: "normal",
        lineHeight: "normal",
        userSelect: "none",
        color: "white",
        paddingLeft: "2px",
        paddingRight: "2px",
        zIndex: 110,
        transition: "opacity .3s ease-in-out",
        backgroundColor: "inherit",
        // these should be separate
        opacity: 0,
        transitionDelay: "0s",
        whiteSpace: "nowrap",
    },
    ".cm-ySelectionCaret:hover > .cm-ySelectionInfo": {
        opacity: 1,
        transitionDelay: "0s",
    },
});

class YRemoteCaretWidget extends cmView.WidgetType {
    constructor(private color: string, private name: string) {
        super();
        this.color = color;
        this.name = name;
    }

    toDOM(): any {
        return dom.element(
            "span",
            [
                pair.create("class", "cm-ySelectionCaret"),
                pair.create(
                    "style",
                    `background-color: ${this.color}; border-color: ${this.color}`
                ),
            ],
            [
                dom.text("\u2060"),
                dom.element("div", [pair.create("class", "cm-ySelectionCaretDot")]),
                dom.text("\u2060"),
                dom.element(
                    "div",
                    [pair.create("class", "cm-ySelectionInfo")],
                    [dom.text(this.name)]
                ),
                dom.text("\u2060"),
            ]
        );
    }

    eq(widget: any) {
        return widget.color === this.color;
    }

    compare(widget: any) {
        return widget.color === this.color;
    }

    updateDOM() {
        return false;
    }

    get estimatedHeight() {
        return -1;
    }

    ignoreEvent() {
        return true;
    }
}

export const remoteCursors = (getRemoteCursors: () => Record<string, RemoteCursor | null>) =>
    cmView.ViewPlugin.fromClass(
        class {
            public decorations: cmView.DecorationSet;

            constructor(view: cmView.EditorView) {
                this.decorations = cmState.RangeSet.of([]);
            }

            /**
             * @param {cmView.ViewUpdate} update
             */
            update(update: cmView.ViewUpdate) {
                const decorations: Array<cmState.Range<cmView.Decoration>> = [];

                // update decorations (remote selections)
                Object.entries(getRemoteCursors()).forEach(([peer, cursor]) => {
                    if (!cursor) return;

                    const { name, from, to, anchor } = cursor;

                    if (from < 0 || to > update.view.state.doc.length) {
                        return;
                    }

                    const color = userColor(peer);
                    const colorLight = `${color}33`;
                    const start = from;
                    const end = to;
                    const startLine = update.view.state.doc.lineAt(start);
                    const endLine = update.view.state.doc.lineAt(end);
                    if (startLine.number === endLine.number) {
                        // selected content in a single line.
                        decorations.push({
                            from: start,
                            to: end,
                            value: cmView.Decoration.mark({
                                attributes: { style: `background-color: ${colorLight}` },
                                class: "cm-ySelection",
                            }),
                        });
                    } else {
                        // selected content in multiple lines
                        // first, render text-selection in the first line
                        decorations.push({
                            from: start,
                            to: startLine.from + startLine.length,
                            value: cmView.Decoration.mark({
                                attributes: { style: `background-color: ${colorLight}` },
                                class: "cm-ySelection",
                            }),
                        });
                        // render text-selection in the last line
                        decorations.push({
                            from: endLine.from,
                            to: end,
                            value: cmView.Decoration.mark({
                                attributes: { style: `background-color: ${colorLight}` },
                                class: "cm-ySelection",
                            }),
                        });
                        for (let i = startLine.number + 1; i < endLine.number; i++) {
                            const linePos = update.view.state.doc.line(i).from;
                            decorations.push({
                                from: linePos,
                                to: linePos,
                                value: cmView.Decoration.line({
                                    attributes: {
                                        style: `background-color: ${colorLight}`,
                                        class: "cm-yLineSelection",
                                    },
                                }),
                            });
                        }
                    }
                    decorations.push({
                        from: anchor,
                        to: anchor,
                        value: cmView.Decoration.widget({
                            side: -1,
                            block: false,
                            widget: new YRemoteCaretWidget(color, name),
                        }),
                    });
                });

                this.decorations = cmView.Decoration.set(decorations, true);
            }
        },
        {
            decorations: (v) => v.decorations,
        }
    );
