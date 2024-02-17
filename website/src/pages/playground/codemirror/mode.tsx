import ReactDOM from "react-dom/client";
import { EditorView, Decoration, DecorationSet, WidgetType } from "@codemirror/view";
import { syntaxTree } from "@codemirror/language";
import {
    Compartment,
    EditorState,
    Extension,
    Facet,
    Range,
    StateEffect,
    StateField,
} from "@codemirror/state";
import { SyntaxNode, Tree } from "@lezer/common";
import { classHighlighter } from "@lezer/highlight";
import { Tooltip } from "../../../components";
import { wippleTags } from "./language";
import { ThemeConfig, defaultThemeConfig } from "./theme";

export const selectionMode = new Compartment();

export const selectionModeFromEnabled = (enabled: boolean, theme: ThemeConfig): Extension =>
    enabled
        ? [
              themeFacet.of(theme),
              blocks,
              blocksListener,
              EditorView.decorations.compute([blocks], (state) => state.field(blocks)),
              EditorView.editable.of(false),
          ]
        : [];

const themeFacet = Facet.define<ThemeConfig, ThemeConfig>({
    combine: (values) => values[values.length - 1] ?? defaultThemeConfig(),
});

const blocksFacet = Facet.define<DecorationSet, DecorationSet>({
    combine: (values) => values[values.length - 1] ?? Decoration.none,
});

const blocks = StateField.define<DecorationSet>({
    create: (state) => computeBlocks(syntaxTree(state), state),
    update: (value, update) => {
        let newValue = value;
        for (const effect of update.effects) {
            if (effect.is(updateBlocks)) {
                newValue = effect.value;
            }
        }

        return newValue;
    },
    provide: (f) => blocksFacet.from(f),
});

const updateBlocks = StateEffect.define<DecorationSet>();

const blocksListener = EditorView.updateListener.of((update) => {
    if (!update.docChanged && !update.viewportChanged) {
        return;
    }

    const tree = syntaxTree(update.state);
    if (syntaxTree(update.startState) === tree) {
        return;
    }

    update.view.dispatch({
        effects: updateBlocks.of(computeBlocks(tree, update.state)),
    });
});

const computeBlocks = (syntaxTree: Tree, state: EditorState) => {
    const theme = state.facet(themeFacet);

    const decorations: Range<Decoration>[] = [];
    syntaxTree.iterate({
        enter: (node) => {
            const { from, to } = node;

            // Decorations may not be empty
            if (from === to) {
                return;
            }

            const terminals = ["Text", "Number", "Keyword", "Operator", "Type", "Name"];
            if (!terminals.includes(node.type.name)) {
                return;
            }

            const code = state.sliceDoc(from, to);

            const widget = new BlockWidget(from, to, code, node.node, theme);
            decorations.push(Decoration.replace({ widget }).range(from, to));
        },
    });

    return Decoration.set(decorations, true);
};

class BlockWidget extends WidgetType {
    constructor(
        public from: number,
        public to: number,
        public code: string,
        public node: SyntaxNode,
        public theme: ThemeConfig,
    ) {
        super();
    }

    eq(other: this) {
        return (
            this.from === other.from &&
            this.to === other.to &&
            this.code === other.code &&
            this.node === other.node &&
            this.theme === other.theme
        );
    }

    toDOM() {
        const container = document.createElement("span");
        ReactDOM.createRoot(container).render(
            <BlockWidgetComponent
                from={this.from}
                to={this.to}
                code={this.code}
                node={this.node}
                theme={this.theme}
            />,
        );

        return container;
    }
}

const BlockWidgetComponent = (props: {
    from: number;
    to: number;
    code: string;
    node: SyntaxNode;
    theme: ThemeConfig;
}) => {
    const tag = wippleTags[props.node.type.name as keyof typeof wippleTags];

    const codeStyles = (tag && classHighlighter.style([tag])) ?? "";

    return (
        <Tooltip
            description={
                <div className="whitespace-nowrap text-sm">
                    <code
                        className={codeStyles}
                        style={{
                            fontFamily: props.theme.fontFamily,
                            fontFeatureSettings: "normal",
                            fontVariationSettings: "normal",
                            fontVariantLigatures: "none",
                        }}
                    >
                        {props.code}
                    </code>
                </div>
            }
        >
            <span className="rounded-lg hover:-m-0.5 hover:border-2 hover:border-gray-100 hover:dark:border-gray-800">
                <span className={codeStyles}>{props.code}</span>
            </span>
        </Tooltip>
    );
};
