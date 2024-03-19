import { Compartment, Extension } from "@codemirror/state";
import { EditorView } from "@codemirror/view";
import { syntaxHighlighting } from "@codemirror/language";
import { classHighlighter } from "@lezer/highlight";

export interface ThemeConfig {
    fontSize: number;
    fontFamily: string;
    highlight: boolean;
}

export const defaultThemeConfig = (): ThemeConfig => ({
    fontSize: 16,
    fontFamily: "JetBrains Mono",
    highlight: true,
});

export const themeFromConfig = (config: ThemeConfig): Extension => [
    EditorView.baseTheme({
        "&.cm-editor.cm-focused": {
            outline: "none",
        },
        ".cm-scroller": {
            fontSize: `${config.fontSize}px`,
            fontFamily: `${config.fontFamily}, monospace`,
            fontFeatureSettings: "normal",
            fontVariationSettings: "normal",
            fontVariantLigatures: "none",
        },
        ".cm-content": {
            padding: "1rem 1rem 0 1rem",
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
    config.highlight ? syntaxHighlighting(classHighlighter) : [],
];

export const theme = new Compartment();
