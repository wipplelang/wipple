import ReactDOM from "react-dom/client";
import {
    EditorView,
    Decoration,
    ViewPlugin,
    ViewUpdate,
    DecorationSet,
    WidgetType,
} from "@codemirror/view";
import { Compartment, Range } from "@codemirror/state";
import { syntaxTree } from "@codemirror/language";
import { Asset, getAsset } from "../assets";

export const assets = new Compartment();

export type AssetClickHandler = (config: { start: number; end: number; asset: Asset }) => void;

export const assetsFromConfig = (config: { disabled: boolean; onClick: AssetClickHandler }) =>
    ViewPlugin.fromClass(
        class {
            decorations: DecorationSet;

            constructor(view: EditorView) {
                this.decorations = getDecorations(view, config);
            }

            update(update: ViewUpdate) {
                if (
                    update.docChanged ||
                    syntaxTree(update.startState) !== syntaxTree(update.state)
                ) {
                    this.decorations = getDecorations(update.view, config);
                }
            }
        },
        {
            decorations: (v) => v.decorations,
            provide: (v) =>
                EditorView.atomicRanges.of(
                    (view) => view.plugin(v)?.decorations ?? Decoration.none,
                ),
        },
    );

const getDecorations = (
    view: EditorView,
    config: { disabled: boolean; onClick: AssetClickHandler },
) => {
    const decorations: Range<Decoration>[] = [];

    syntaxTree(view.state).iterate({
        enter: (node) => {
            const { from, to } = node;

            if (node.type.name !== "Asset") {
                return;
            }

            let code = view.state.sliceDoc(from, to);

            if (code.includes("\n")) {
                return;
            }

            code = code.slice(1, code.length - 1); // remove brackets

            const asset = getAsset(code);
            if (!asset) {
                return;
            }

            decorations.push(
                Decoration.replace({
                    widget: new AssetWidget(from, to, asset, config),
                }).range(from, to),
            );
        },
    });

    return Decoration.set(decorations, true);
};

class AssetWidget extends WidgetType {
    private root?: ReactDOM.Root;

    constructor(
        public from: number,
        public to: number,
        public asset: Asset,
        public config: { disabled: boolean; onClick: AssetClickHandler },
    ) {
        super();
    }

    eq(other: this) {
        return (
            this.from === other.from &&
            this.to === other.to &&
            this.asset === other.asset &&
            this.config === other.config
        );
    }

    toDOM() {
        const container = document.createElement("span");

        this.root = ReactDOM.createRoot(container);
        this.root.render(
            <AssetWidgetComponent
                asset={this.asset}
                disabled={this.config.disabled}
                onClick={(asset) =>
                    this.config.onClick({
                        start: this.from,
                        end: this.to,
                        asset,
                    })
                }
            />,
        );

        return container;
    }

    destroy() {
        requestAnimationFrame(() => {
            this.root?.unmount();
        });
    }
}

const AssetWidgetComponent = (props: {
    asset: Asset;
    disabled: boolean;
    onClick: (asset: Asset) => void;
}) => (
    <Asset disabled={props.disabled} onClick={props.onClick}>
        {props.asset}
    </Asset>
);
