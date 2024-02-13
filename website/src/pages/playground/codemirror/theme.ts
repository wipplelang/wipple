import { Compartment, EditorState, Extension, Facet } from "@codemirror/state";
import { EditorView } from "@codemirror/view";

export interface ThemeConfig {
    dark: boolean;
    fontSize: number;
    fontFamily: string;
}

export const defaultThemeConfig = (): ThemeConfig => ({
    dark: window.matchMedia("(prefers-color-scheme: dark)").matches,
    fontSize: 14,
    fontFamily: "JetBrains Mono",
});

const themeFromConfig = (config: ThemeConfig): Extension => [
    EditorView.baseTheme({
        "&.cm-editor": {
            fontSize: config.fontSize,
            fontFamily: `${config.fontFamily}, monospace`,
            fontVariantLigatures: "none",
        },
        "&.cm-editor.cm-focused": {
            outline: "none",
        },
        ".cm-scroller": {
            fontFamily: "inherit",
        },
        ".cm-content": {
            padding: "1rem",
        },
        ".cm-line": {
            padding: 0,
        },
        ".cm-placeholder": {
            fontStyle: "italic",
        },
        ".cm-tooltip-hover": {
            border: "none",
            backgroundColor: "unset",
        },
    }),
];

const themeCompartment = new Compartment();

export const theme = Facet.define<ThemeConfig, ThemeConfig>({
    combine: (configs) => configs[configs.length - 1] ?? defaultThemeConfig(),
    enables: themeCompartment.of(themeFromConfig(defaultThemeConfig())),
});

export const updateTheme = EditorState.transactionExtender.of((tr) => {
    const config = tr.state.facet(theme);

    return {
        effects: themeCompartment.reconfigure(themeFromConfig(config)),
    };
});
