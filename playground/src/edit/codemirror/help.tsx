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
import { NodeType, SyntaxNode, Tree } from "@lezer/common";
import { classHighlighter } from "@lezer/highlight";
import { Markdown, Tooltip } from "../../components";
import { wippleTags } from "./language";
import { ThemeConfig, defaultThemeConfig, highlightCategories } from "./theme";
import { Help } from "../../models";
import { useEffect, useState } from "react";
import { isAsset } from "../assets";

export const displayHelp = new Compartment();

export const displayHelpFromEnabled = (
    enabled: boolean,
    theme: ThemeConfig,
    help: (position: number, code: string) => Promise<Help | undefined>,
    highlightItems: Record<string, any>,
    onClick: (help: Help) => void,
): Extension =>
    enabled
        ? [
              configFacet.of({ theme, help, onClick, highlightItems }),
              helpDecorations,
              helpDecorationsListener,
              EditorView.decorations.compute(
                  [helpDecorations],
                  (state) => state.field(helpDecorations) ?? Decoration.none,
              ),
          ]
        : [];

interface Config {
    theme: ThemeConfig;
    help: (position: number, code: string) => Promise<Help | undefined>;
    onClick: (help: Help) => void;
    highlightItems: Record<string, any>;
}

const configFacet = Facet.define<Config, Config>({
    combine: (values) => values[values.length - 1] ?? defaultThemeConfig(),
});

const helpDecorationsFacet = Facet.define<DecorationSet, DecorationSet>({
    combine: (values) => values[values.length - 1] ?? Decoration.none,
});

const helpDecorations = StateField.define<DecorationSet | undefined>({
    create: () => undefined,
    update: (value, update) => {
        let newValue = value?.map(update.changes);
        for (const effect of update.effects) {
            if (effect.is(updateHelpDecorations)) {
                newValue = effect.value;
            }
        }

        return newValue;
    },
    provide: (f) => helpDecorationsFacet.from(f),
});

const updateHelpDecorations = StateEffect.define<DecorationSet>();

const helpDecorationsListener = EditorView.updateListener.of((update) => {
    const tree = syntaxTree(update.state);
    if (syntaxTree(update.startState) === tree && update.state.field(helpDecorations) != null) {
        return;
    }

    (async () => {
        const helpDecorations = await computeHelpDecorations(tree, update.state);

        update.view.dispatch({
            effects: updateHelpDecorations.of(helpDecorations),
        });
    })();
});

const computeHelpDecorations = async (syntaxTree: Tree, state: EditorState) => {
    const terminals = ["Keyword", "Operator", "Type", "Name"];

    const { theme, help, onClick, highlightItems } = state.facet(configFacet);

    const decorations: Range<Decoration>[] = [];

    const queue: {
        from: number;
        to: number;
        type: NodeType;
        node: SyntaxNode;
    }[] = [];
    syntaxTree.iterate({
        enter: (node) => {
            const { from, to } = node;

            // Decorations may not be empty
            if (from === to) {
                return;
            }

            const code = state.sliceDoc(from, to);

            if (
                isAsset(code) || // assets are handled separately
                !terminals.includes(node.type.name)
            ) {
                return;
            }

            queue.push({ from: from, to: to, type: node.type, node: { ...node.node } });
        },
    });

    for (const { from, to, type, node } of queue) {
        const code = state.sliceDoc(from, to);

        const highlight = type.name === "Name" ? highlightItems[code] : undefined;

        const widget = new HelpWidget(
            from,
            to,
            code,
            { ...node },
            theme,
            await help(from, code),
            highlight,
            onClick,
        );

        decorations.push(Decoration.replace({ widget }).range(from, to));
    }

    return Decoration.set(decorations, true);
};

class HelpWidget extends WidgetType {
    private root?: ReactDOM.Root;

    constructor(
        public from: number,
        public to: number,
        public code: string,
        public node: SyntaxNode,
        public theme: ThemeConfig,
        public help: Help | undefined,
        public highlight: any | undefined,
        public onClick: (help: Help) => void,
    ) {
        super();
    }

    eq(other: this) {
        return (
            this.from === other.from &&
            this.to === other.to &&
            this.code === other.code &&
            this.node === other.node &&
            this.theme === other.theme &&
            this.help === other.help &&
            this.highlight === other.highlight &&
            this.onClick === other.onClick
        );
    }

    toDOM() {
        // HACK: Prevent flashing while React renders the actual component
        const [codeClassName, codeStyle] = codeStyles(this.node, this.theme);
        const container = document.createElement("span");
        container.className = codeClassName;
        Object.assign(container.style, codeStyle);
        container.innerText = this.code;

        this.root = ReactDOM.createRoot(container);
        this.root.render(
            <HelpWidgetComponent
                from={this.from}
                to={this.to}
                code={this.code}
                node={this.node}
                theme={this.theme}
                help={this.help}
                highlight={this.highlight}
                onClick={this.onClick}
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

const codeStyles = (node: SyntaxNode, theme: ThemeConfig) => {
    const tag = wippleTags[node.type.name as keyof typeof wippleTags];

    const className = (tag && classHighlighter.style([tag])) ?? "";

    return [
        className,
        {
            fontFamily: theme.fontFamily,
            fontFeatureSettings: "normal",
            fontVariationSettings: "normal",
            fontVariantLigatures: "none",
        },
    ] as const;
};

const HelpWidgetComponent = (props: {
    from: number;
    to: number;
    code: string;
    node: SyntaxNode;
    theme: ThemeConfig;
    help: Help | undefined;
    highlight: any | undefined;
    onClick: (help: Help) => void;
}) => {
    const [codeClassName, codeStyle] = codeStyles(props.node, props.theme);

    const renderedCode = (
        <code className={codeClassName} style={codeStyle}>
            {props.code}
        </code>
    );

    const [isAnimating, setAnimating] = useState(true);

    useEffect(() => {
        requestAnimationFrame(() => {
            setAnimating(false);
        });
    }, []);

    let className = props.help == null ? (isAnimating ? "" : "opacity-25") : "cursor-pointer";
    if (props.highlight?.category && highlightCategories[props.highlight.category]) {
        className += ` ${highlightCategories[props.highlight.category]} ${
            props.highlight.icon ? "pr-1 rounded-r-[4px]" : "px-1 rounded-[4px]"
        }`;
    } else {
        className += "px-1 rounded-[4px]";
    }

    return (
        <Tooltip
            disabled={props.help == null}
            description={
                <div className="whitespace-nowrap text-sm">
                    {props.help ? <Markdown>{props.help.summary}</Markdown> : null}
                </div>
            }
            onClick={() => {
                if (props.help) {
                    props.onClick(props.help);
                }
            }}
        >
            <span className={className}>{renderedCode}</span>
        </Tooltip>
    );
};
