// Adapted from https://github.com/saminzadeh/codemirror-extension-inline-suggestion

import {
    ViewPlugin,
    DecorationSet,
    ViewUpdate,
    Decoration,
    WidgetType,
    keymap,
} from "@codemirror/view";
import {
    StateEffect,
    Prec,
    StateField,
    EditorState,
    EditorSelection,
    TransactionSpec,
} from "@codemirror/state";

// Current state of the autosuggestion
const InlineSuggestionState = StateField.define<{ pos: number; text: string } | null>({
    create() {
        return null;
    },
    update(prev, tr) {
        const inlineSuggestion = tr.effects.find((e) => e.is(InlineSuggestionEffect));
        if (!inlineSuggestion) return prev;

        return inlineSuggestion.value;
    },
});

const InlineSuggestionEffect = StateEffect.define<{ pos: number; text: string } | null>();

/**
 * Provides a suggestion for the next word
 */
function inlineSuggestionDecoration(pos: number, prefix: string) {
    const widgets = [];
    const w = Decoration.widget({
        widget: new InlineSuggestionWidget(prefix),
        side: 1,
    });
    widgets.push(w.range(pos));
    return Decoration.set(widgets);
}

class InlineSuggestionWidget extends WidgetType {
    suggestion: string;
    constructor(suggestion: string) {
        super();
        this.suggestion = suggestion;
    }
    toDOM() {
        const div = document.createElement("span");
        div.style.opacity = "0.4";
        div.className = "cm-inline-suggestion";
        div.textContent = this.suggestion;
        return div;
    }
}

type InlineFetchFn = (update: ViewUpdate) => Promise<{ pos: number; text: string } | null>;

export const fetchSuggestion = (fetchFn: InlineFetchFn) =>
    ViewPlugin.fromClass(
        class Plugin {
            async update(update: ViewUpdate) {
                if (!update.docChanged && !update.focusChanged && !update.selectionSet) return;

                const result = await fetchFn(update);
                update.view.dispatch({
                    effects: InlineSuggestionEffect.of(result),
                });
            }
        }
    );

const renderInlineSuggestionPlugin = ViewPlugin.fromClass(
    class Plugin {
        decorations: DecorationSet;
        constructor() {
            // Empty decorations
            this.decorations = Decoration.none;
        }
        update(update: ViewUpdate) {
            if (
                !update.view.hasFocus ||
                update.state.doc.length === 0 ||
                update.selectionSet ||
                !update.state.selection.main.empty
            ) {
                this.decorations = Decoration.none;
                return;
            }

            const suggestion = update.state.field(InlineSuggestionState);
            if (!suggestion) {
                this.decorations = Decoration.none;
                return;
            }

            this.decorations = inlineSuggestionDecoration(suggestion.pos, suggestion.text);
        }
    },
    {
        decorations: (v) => {
            return v.decorations;
        },
    }
);

const inlineSuggestionKeymap = Prec.highest(
    keymap.of([
        {
            key: "Tab",
            run: (view) => {
                const suggestion = view.state.field(InlineSuggestionState);

                // If there is no suggestion, do nothing and let the default keymap handle it
                if (!suggestion) {
                    return false;
                }

                view.dispatch({
                    ...insertCompletionText(
                        view.state,
                        suggestion.text,
                        suggestion.pos,
                        suggestion.pos
                    ),
                    effects: InlineSuggestionEffect.of(null),
                });

                return true;
            },
        },
    ])
);

function insertCompletionText(
    state: EditorState,
    text: string,
    from: number,
    to: number
): TransactionSpec {
    return {
        ...state.changeByRange((range) => {
            if (range == state.selection.main)
                return {
                    changes: { from: from, to: to, insert: text },
                    range: EditorSelection.cursor(from + text.length),
                };
            const len = to - from;
            if (
                !range.empty ||
                (len && state.sliceDoc(range.from - len, range.from) != state.sliceDoc(from, to))
            )
                return { range };
            return {
                changes: { from: range.from - len, to: range.from, insert: text },
                range: EditorSelection.cursor(range.from - len + text.length),
            };
        }),
        userEvent: "input.complete",
    };
}

type InlineSuggestionOptions = {
    fetchFn: InlineFetchFn;
};

export function inlineSuggestion(options: InlineSuggestionOptions) {
    const { fetchFn } = options;
    return [
        InlineSuggestionState,
        fetchSuggestion(fetchFn),
        renderInlineSuggestionPlugin,
        inlineSuggestionKeymap,
    ];
}
