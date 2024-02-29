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
import { Asset } from "../assets";

export const assets = new Compartment();

export type AssetClickHandler = (config: {
    start: number;
    end: number;
    type: string;
    value: string;
}) => void;

export const assetsFromConfig = (config: { onClick: AssetClickHandler }) =>
    ViewPlugin.fromClass(
        class {
            decorations: DecorationSet;

            constructor(view: EditorView) {
                this.decorations = getDecorations(view, config.onClick);
            }

            update(update: ViewUpdate) {
                if (
                    update.docChanged ||
                    syntaxTree(update.startState) !== syntaxTree(update.state)
                ) {
                    this.decorations = getDecorations(update.view, config.onClick);
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

const getDecorations = (view: EditorView, onClick: AssetClickHandler) => {
    const decorations: Range<Decoration>[] = [];

    syntaxTree(view.state).iterate({
        enter: (node) => {
            const { from, to } = node;

            if (node.type.name !== "Asset") {
                return;
            }

            const code = view.state.sliceDoc(from, to);

            if (code.includes("\n")) {
                return;
            }

            decorations.push(
                Decoration.replace({
                    widget: new AssetWidget(from, to, code, onClick),
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
        public code: string,
        public onClick: AssetClickHandler,
    ) {
        super();
    }

    eq(other: this) {
        return (
            this.from === other.from &&
            this.to === other.to &&
            this.code === other.code &&
            this.onClick === other.onClick
        );
    }

    toDOM() {
        const container = document.createElement("span");

        this.root = ReactDOM.createRoot(container);
        this.root.render(
            <AssetWidgetComponent
                code={this.code}
                onClick={(type, value) =>
                    this.onClick({
                        start: this.from,
                        end: this.to,
                        type,
                        value,
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
    code: string;
    onClick: (type: string, value: string) => void;
}) => <Asset onClick={props.onClick}>{props.code.slice(1, props.code.length - 1)}</Asset>;
