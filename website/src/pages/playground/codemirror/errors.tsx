import ReactDOM from "react-dom/client";
import { EditorView, Decoration, DecorationSet, WidgetType } from "@codemirror/view";
import {
    Compartment,
    EditorState,
    Extension,
    Facet,
    StateEffect,
    StateField,
} from "@codemirror/state";
import { ThemeConfig, defaultThemeConfig } from "./theme";
import { Diagnostic, Fix, Label } from "../../../models";
import { Markdown } from "../../../components";

export const errors = new Compartment();

export const errorsFromConfig = (config: Config): Extension =>
    config.enabled
        ? [
              configFacet.of(config),
              errorBlocks,
              EditorView.decorations.compute([errorBlocks], (state) => state.field(errorBlocks)),
              EditorView.editable.of(!config.enabled),
          ]
        : [];

interface Config {
    enabled: boolean;
    diagnostics: Diagnostic[];
    onApplyFix: (fix: Fix) => void;
    editorView: () => EditorView | null;
    theme: ThemeConfig;
}

const configFacet = Facet.define<Config, Config>({
    combine: (values) => values[values.length - 1] ?? defaultThemeConfig(),
});

const errorBlocksFacet = Facet.define<DecorationSet, DecorationSet>({
    combine: (values) => values[values.length - 1] ?? Decoration.none,
});

const errorBlocks = StateField.define<DecorationSet>({
    create: (state) => computeErrorBlocks(state.facet(configFacet), state),
    update: (value, update) => {
        let newValue = value;
        for (const effect of update.effects) {
            if (effect.is(updateErrorBlocks)) {
                newValue = effect.value;
            }
        }

        return newValue;
    },
    provide: (f) => errorBlocksFacet.from(f),
});

const updateErrorBlocks = StateEffect.define<DecorationSet>();

const computeErrorBlocks = (config: Config, state: EditorState) => {
    const editorView = config.editorView();
    if (!editorView) {
        return Decoration.none;
    }

    const editorRect = editorView.dom.getBoundingClientRect();

    const decorations = config.diagnostics.flatMap((diagnostic) => {
        const primaryLine = state.doc.lineAt(diagnostic.primaryLabel.span.start);

        const primaryLeft = () =>
            editorView.coordsAtPos(diagnostic.primaryLabel.span.start)!.left - editorRect.left;

        return [
            Decoration.mark({
                class: `cm-diagnostic-highlight ${
                    diagnostic.error ? "text-red-500" : "text-yellow-500"
                }`,
            }).range(diagnostic.primaryLabel.span.start, diagnostic.primaryLabel.span.end),
            Decoration.widget({
                block: true,
                widget: new ErrorBlockWidget(
                    diagnostic.error,
                    true,
                    diagnostic.primaryLabel,
                    diagnostic.fix,
                    primaryLeft,
                    config.theme,
                    () => config.onApplyFix(diagnostic.fix!),
                ),
            }).range(primaryLine.to + 1, primaryLine.to + 1),
            ...diagnostic.secondaryLabels.flatMap((label) => {
                const secondaryLine = state.doc.lineAt(label.span.start);

                const secondaryLeft = () =>
                    editorView.coordsAtPos(label.span.start)!.left - editorRect.left;

                return [
                    Decoration.mark({
                        class: `cm-diagnostic-highlight ${
                            diagnostic.error
                                ? "text-red-400 dark:text-red-600"
                                : "text-yellow-400 dark:text-yellow-600"
                        }`,
                    }).range(label.span.start, label.span.end),
                    Decoration.widget({
                        block: true,
                        widget: new ErrorBlockWidget(
                            diagnostic.error,
                            false,
                            label,
                            null,
                            secondaryLeft,
                            config.theme,
                        ),
                    }).range(secondaryLine.to + 1, secondaryLine.to + 1),
                ];
            }),
        ];
    });

    return Decoration.set(decorations, true);
};

class ErrorBlockWidget extends WidgetType {
    private root?: ReactDOM.Root;

    constructor(
        public error: boolean,
        public primary: boolean,
        public label: Label,
        public fix: Fix | null,
        public left: () => number,
        public theme: ThemeConfig,
        public onClickFix?: () => void,
    ) {
        super();
    }

    eq(other: this) {
        return (
            this.error === other.error &&
            this.primary === other.primary &&
            this.label === other.label &&
            this.fix === other.fix &&
            this.left === other.left &&
            this.theme === other.theme &&
            this.onClickFix === other.onClickFix
        );
    }

    toDOM() {
        const container = document.createElement("span");
        this.root = ReactDOM.createRoot(container);
        this.root.render(
            <BlockWidgetComponent
                error={this.error}
                primary={this.primary}
                label={this.label}
                fix={this.fix}
                left={this.left}
                theme={this.theme}
                onClickFix={this.onClickFix}
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

const BlockWidgetComponent = (props: {
    error: boolean;
    primary: boolean;
    label: Label;
    fix: Fix | null;
    left: () => number;
    theme: ThemeConfig;
    onClickFix?: () => void;
}) => {
    return (
        <div style={{ marginLeft: props.left() }}>
            <div className="-ml-4 pb-4">
                <div className="flex flex-col w-fit ui-font text-black dark:text-gray-50">
                    <Markdown>{props.label.message}</Markdown>
                </div>
            </div>
        </div>
    );
};
