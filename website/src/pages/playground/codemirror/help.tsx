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
import { Markdown, Tooltip } from "../../../components";
import { wippleTags } from "./language";
import { ThemeConfig, defaultThemeConfig } from "./theme";
import { Help } from "../../../models";
import { useEffect, useState } from "react";

export const displayHelp = new Compartment();

export const displayHelpFromEnabled = (
    enabled: boolean,
    theme: ThemeConfig,
    help: (code: string) => Help | undefined,
    onClick: (help: Help) => void,
): Extension =>
    enabled
        ? [
              configFacet.of({ theme, help, onClick }),
              helpDecorations,
              helpDecorationsListener,
              EditorView.decorations.compute([helpDecorations], (state) =>
                  state.field(helpDecorations),
              ),
          ]
        : [];

interface Config {
    theme: ThemeConfig;
    help: (code: string) => Help | undefined;
    onClick: (help: Help) => void;
}

const configFacet = Facet.define<Config, Config>({
    combine: (values) => values[values.length - 1] ?? defaultThemeConfig(),
});

const helpDecorationsFacet = Facet.define<DecorationSet, DecorationSet>({
    combine: (values) => values[values.length - 1] ?? Decoration.none,
});

const helpDecorations = StateField.define<DecorationSet>({
    create: (state) => computeHelpDecorations(syntaxTree(state), state),
    update: (value, update) => {
        let newValue = value;
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
    if (!update.docChanged && !update.viewportChanged) {
        return;
    }

    const tree = syntaxTree(update.state);
    if (syntaxTree(update.startState) === tree) {
        return;
    }

    update.view.dispatch({
        effects: updateHelpDecorations.of(computeHelpDecorations(tree, update.state)),
    });
});

const computeHelpDecorations = (syntaxTree: Tree, state: EditorState) => {
    const { theme, help, onClick } = state.facet(configFacet);

    const decorations: Range<Decoration>[] = [];
    syntaxTree.iterate({
        enter: (node) => {
            const { from, to } = node;

            // Decorations may not be empty
            if (from === to) {
                return;
            }

            const code = state.sliceDoc(from, to);

            const terminals = [
                "Comment",
                "Text",
                "Number",
                "Asset",
                "Keyword",
                "Operator",
                "Type",
                "Name",
            ];

            const brackets = ["[", "]", "{", "}", "(", ")"];

            if (!terminals.includes(node.type.name) && !brackets.includes(code)) {
                return;
            }

            const widget = new HelpWidget(from, to, code, node.node, theme, help(code), onClick);
            decorations.push(Decoration.replace({ widget }).range(from, to));
        },
    });

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
            <span
                className={`rounded-lg transition-opacity ${
                    props.help == null
                        ? isAnimating
                            ? ""
                            : "opacity-25"
                        : "hover:-m-0.5 hover:border-2 hover:border-gray-100 hover:dark:border-gray-800 cursor-pointer"
                }`}
            >
                {renderedCode}
            </span>
        </Tooltip>
    );
};
