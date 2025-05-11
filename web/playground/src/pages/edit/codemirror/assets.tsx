import ReactDOM from "react-dom/client";
import {
    EditorView,
    Decoration,
    ViewPlugin,
    ViewUpdate,
    DecorationSet,
    WidgetType,
} from "@codemirror/view";
import { Compartment, Range, RangeSet, StateField } from "@codemirror/state";
import { syntaxTree } from "@codemirror/language";
import { Asset, getAsset } from "../assets";
import { ThemeConfig, highlightCategories } from "./theme";
import { MaterialSymbol } from "react-material-symbols";

export const assets = new Compartment();

export type AssetClickHandler = (config: { start: number; end: number; asset: Asset }) => void;

export const assetsFromConfig = (config: {
    disabled: boolean;
    onClick?: AssetClickHandler;
    highlightItems: Record<string, any>;
    theme: ThemeConfig;
}) =>
    ViewPlugin.fromClass(
        class {
            nonatomicDecorations: DecorationSet;
            atomicDecorations: DecorationSet;

            constructor(view: EditorView) {
                const [nonatomicDecorations, atomicDecorations] = getDecorations(view, config);
                this.nonatomicDecorations = nonatomicDecorations;
                this.atomicDecorations = atomicDecorations;
            }

            update(update: ViewUpdate) {
                if (
                    update.docChanged ||
                    syntaxTree(update.startState) !== syntaxTree(update.state)
                ) {
                    const [nonatomicDecorations, atomicDecorations] = getDecorations(
                        update.view,
                        config,
                    );

                    this.nonatomicDecorations = nonatomicDecorations;
                    this.atomicDecorations = atomicDecorations;
                }
            }
        },
        {
            decorations: (v) => RangeSet.join([v.nonatomicDecorations, v.atomicDecorations]),
            provide: (v) =>
                EditorView.atomicRanges.of(
                    (view) => view.plugin(v)?.atomicDecorations ?? Decoration.none,
                ),
        },
    );

const getDecorations = (
    view: EditorView,
    config: {
        disabled: boolean;
        onClick?: AssetClickHandler;
        highlightItems: Record<string, any>;
        theme: ThemeConfig;
    },
): [DecorationSet, DecorationSet] => {
    const nonatomicDecorations: Range<Decoration>[] = [];
    const atomicDecorations: Range<Decoration>[] = [];
    syntaxTree(view.state).iterate({
        enter: (node) => {
            const { from, to } = node;
            let code = view.state.sliceDoc(from, to);

            switch (node.type.name) {
                case "Name": {
                    const highlight = config.highlightItems[code];

                    if (highlight?.category && highlightCategories[highlight.category]) {
                        const className = highlightCategories[highlight.category];

                        nonatomicDecorations.push(
                            Decoration.mark({
                                class: `${className} ${
                                    highlight.icon ? "pr-1 rounded-r-[4px]" : "px-1 rounded-[4px]"
                                }`,
                            }).range(from, to),
                        );

                        if (highlight.icon) {
                            const widget = new HighlightIconWidget(
                                from,
                                highlight.icon,
                                className,
                                config.theme.fontSize,
                            );

                            nonatomicDecorations.push(
                                Decoration.widget({ widget, side: -1 }).range(from),
                            );
                        }
                    }

                    break;
                }
                case "Asset": {
                    if (code.includes("\n")) {
                        break;
                    }

                    code = code.slice(1, code.length - 1); // remove brackets

                    const asset = getAsset(code);
                    if (asset) {
                        const widget = new AssetWidget(asset, {
                            ...config,
                            onClick: (asset) => config.onClick?.({ start: from, end: to, asset }),
                        });

                        atomicDecorations.push(Decoration.replace({ widget }).range(from, to));
                    }

                    break;
                }
                default:
                    break;
            }
        },
    });

    return [Decoration.set(nonatomicDecorations, true), Decoration.set(atomicDecorations, true)];
};

class HighlightIconWidget extends WidgetType {
    private root?: ReactDOM.Root;

    constructor(
        public index: number,
        public icon: string,
        public className: string,
        public fontSize: number,
    ) {
        super();
    }

    toDOM() {
        const container = document.createElement("span");

        this.root = ReactDOM.createRoot(container);
        this.root.render(
            <HighlightIconWidgetComponent
                icon={this.icon}
                className={this.className}
                fontSize={this.fontSize}
            />,
        );

        return container;
    }
}

const HighlightIconWidgetComponent = (props: {
    icon: string;
    fontSize: number;
    className: string;
}) => (
    <span
        className={`inline-block relative w-[calc(1rem+4px)] align-text-bottom rounded-l-[4px] ${props.className}`}
        style={{ height: `${props.fontSize}pt` }}
    >
        <div className="flex items-center justify-center absolute inset-0 left-[2px]">
            <MaterialSymbol icon={props.icon.replace("-", "_") as any} />
        </div>
    </span>
);

class AssetWidget extends WidgetType {
    private root?: ReactDOM.Root;

    constructor(
        public asset: Asset,
        public config: { disabled: boolean; onClick?: (asset: Asset) => void },
    ) {
        super();
    }

    toDOM() {
        const container = document.createElement("span");

        this.root = ReactDOM.createRoot(container);
        this.root.render(
            <AssetWidgetComponent
                asset={this.asset}
                disabled={this.config.disabled}
                onClick={this.config.onClick}
            />,
        );

        return container;
    }
}

const AssetWidgetComponent = (props: {
    asset: Asset;
    disabled: boolean;
    onClick?: (asset: Asset) => void;
}) => (
    <Asset disabled={props.disabled} onClick={props.onClick}>
        {props.asset}
    </Asset>
);
