import ReactDOM from "react-dom/client";
import {
    EditorView,
    Decoration,
    ViewPlugin,
    ViewUpdate,
    DecorationSet,
    WidgetType,
    MatchDecorator,
} from "@codemirror/view";
import { Compartment, StateEffect, StateField } from "@codemirror/state";
import { Asset, getAsset } from "../assets";
import { ThemeConfig, highlightCategories } from "./theme";

export const assets = new Compartment();

export type AssetClickHandler = (config: { start: number; end: number; asset: Asset }) => void;

const nameRegex = /\w+(?:\-\w+)*/g;
const assetRegex = /\[([^\]]*)\]/g;

export const assetsFromConfig = (config: {
    disabled: boolean;
    onClick?: AssetClickHandler;
    highlightItems: Record<string, any>;
    theme: ThemeConfig;
}) => [
    markRegex(nameRegex, (_from, _to, [code]) => {
        const highlight = config.highlightItems[code];
        if (!highlight?.category || !highlightCategories[highlight.category]) {
            return [];
        }

        return [
            {
                decoration: () =>
                    markDecoration(
                        `${highlightCategories[highlight.category]} tok-highlight`,
                        highlight.icon
                            ? `--highlight-icon: '${highlight.icon.replaceAll("-", "_")}';`
                            : "",
                    ),
            },
        ];
    }),
    markRegex(
        assetRegex,
        (from, to, [_code, assetString]) => {
            const asset = getAsset(assetString);
            if (!asset) {
                return [];
            }

            return [
                {
                    decoration: () =>
                        componentDecoration(() => (
                            <Asset
                                disabled={config.disabled}
                                onClick={(asset) =>
                                    config.onClick?.({ start: from, end: to, asset })
                                }
                            >
                                {asset}
                            </Asset>
                        )),
                },
            ];
        },
        { atomic: true },
    ),
];

const markRegex = (
    regex: RegExp,
    decorate: (
        from: number,
        to: number,
        match: RegExpMatchArray,
        view: EditorView,
    ) => { decoration: () => Decoration }[],
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

            const decorations = decorate(from, to, match, view);

            for (const { decoration: getDecoration } of decorations) {
                const string = view.state.sliceDoc(from, to);
                created.add(string);

                let decoration = cache[string];
                if (!decoration) {
                    decoration = getDecoration();
                    cache[string] = decoration;
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

const markDecoration = (className: string, style?: string) =>
    Decoration.mark({
        class: className,
        attributes: style ? { style } : undefined,
        inclusive: true,
    });

const componentDecoration = (component: () => JSX.Element) =>
    Decoration.replace({
        widget: new (class extends WidgetType {
            toDOM() {
                const container = document.createElement("span");

                const root = ReactDOM.createRoot(container);
                root.render(component());

                return container;
            }
        })(),
    });
