import { Prec, RangeSet, StateEffect, StateField } from "@codemirror/state";
import {
    Decoration,
    type DecorationSet,
    EditorView,
    MatchDecorator,
    ViewPlugin,
    ViewUpdate,
    WidgetType,
} from "@codemirror/view";

export const markDecoration = (className: string, style?: string) =>
    Decoration.mark({
        class: className,
        attributes: style ? { style } : undefined,
        inclusive: true,
    });

export const elementDecoration = (element: HTMLElement) =>
    Decoration.replace({
        widget: new (class extends WidgetType {
            toDOM = () => element;
        })(),
    });

export const accessoryDecoration = (element: HTMLElement) =>
    Decoration.widget({
        widget: new (class extends WidgetType {
            toDOM = () => element;
        })(),
    });

export const blockDecoration = (element: HTMLElement) =>
    Decoration.widget({
        widget: new (class extends WidgetType {
            toDOM = () => element;
        })(),
        block: true,
        side: 1,
    });

export const markRegex = (
    regex: RegExp,
    decorate: (
        lineNumber: number,
        lineFrom: number,
        lineTo: number,
        match: RegExpMatchArray,
        view: EditorView,
    ) => {
        decoration: () => Decoration;
        index?: number;
    }[],
    options: { atomic?: boolean } = {},
) => {
    type Cache = Record<string, Decoration>;

    const updateCacheEffect = StateEffect.define<Cache>();

    const cacheField = StateField.define<Cache>({
        create: () => ({}),
        update: (value, transaction) => {
            for (const effect of transaction.effects) {
                if (effect.is(updateCacheEffect)) {
                    value = effect.value;
                }
            }

            return value;
        },
    });

    const created = new Set<string>();

    const decorator = new MatchDecorator({
        regexp: regex,
        decorate: (add, from, to, match, view) => {
            const cache = view.state.field(cacheField);

            // MatchDecorator updates decorations per-line, so make positions
            // relative to the current line. When a decoration needs to update
            // the document, it will have the right line number and relative
            // positions, and the actual document positions can be computed from
            // those. When the line is edited, MatchDecorator will recompute the
            // line's decorations and thus the relative positions too.
            const line = view.state.doc.lineAt(from);
            const lineFrom = from - line.from;
            const lineTo = to - line.from;

            const decorations = decorate(line.number, lineFrom, lineTo, match, view);

            for (const { decoration: getDecoration, index } of decorations) {
                const key = `${line.number}:${lineFrom}-${lineTo}`;
                created.add(key);

                let decoration = cache[key];
                if (!decoration) {
                    decoration = getDecoration();
                    cache[key] = decoration;
                }

                // For accessory decorations, like the number widget
                if (index != null) {
                    // Fix both positions to the single index
                    to = from + index;
                    from = to;
                }

                add(from, to, decoration);
            }
        },
    });

    return [
        cacheField,
        ViewPlugin.fromClass(
            class {
                public decorations: DecorationSet;

                constructor(view: EditorView) {
                    this.decorations = decorator.createDeco(view);
                }

                update(update: ViewUpdate) {
                    this.decorations = decorator.updateDeco(update, this.decorations);

                    // Remove old decorations
                    const cache = update.state.field(cacheField);
                    for (const key in cache) {
                        if (!created.has(key)) {
                            delete cache[key];
                        }
                    }

                    created.clear();
                }
            },
            {
                decorations: (instance) => instance.decorations,
                provide: (plugin) =>
                    EditorView.atomicRanges.of(
                        (view) =>
                            (options.atomic && view.plugin(plugin)?.decorations) || Decoration.none,
                    ),
            },
        ),
    ];
};

export const markRange = (from: number, to: number, decoration: () => Decoration) =>
    Prec.lowest(
        ViewPlugin.fromClass(class {}, {
            decorations: () => RangeSet.of([decoration().range(from, to)]),
        }),
    );
