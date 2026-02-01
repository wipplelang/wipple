import { Prec, RangeSet } from "@codemirror/state";
import {
    Decoration,
    type DecorationSet,
    EditorView,
    MatchDecorator,
    ViewPlugin,
    ViewUpdate,
    WidgetType,
} from "@codemirror/view";

export const markDecoration = (className: string, style?: string, attributes = {}) =>
    Decoration.mark({
        class: className,
        attributes: style ? { style, ...attributes } : attributes,
        inclusive: true,
    });

export const lineDecoration = (className: string) =>
    Decoration.line({
        attributes: { class: className },
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
        match: RegExpMatchArray,
        view: EditorView,
        from: number,
        to: number,
    ) => {
        decoration: () => Decoration;
        index?: number;
    }[],
    options: { atomic?: boolean } = {},
) => {
    const decorator = new MatchDecorator({
        regexp: regex,
        decorate: (add, from, to, match, view) => {
            const decorations = decorate(match, view, from, to);

            for (const { decoration, index } of decorations) {
                // For accessory decorations, like the number widget
                if (index != null) {
                    // Fix both positions to the single index
                    to = from + index;
                    from = to;
                }

                add(from, to, decoration());
            }
        },
    });

    return ViewPlugin.fromClass(
        class {
            public decorations: DecorationSet;

            constructor(view: EditorView) {
                this.decorations = decorator.createDeco(view);
            }

            update(update: ViewUpdate) {
                this.decorations = decorator.updateDeco(update, this.decorations);
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
    );
};

export const markRange = (from: number, to: number, decoration: () => Decoration) =>
    Prec.highest(
        ViewPlugin.fromClass(class {}, {
            decorations: () => RangeSet.of([decoration().range(from, to)]),
        }),
    );
