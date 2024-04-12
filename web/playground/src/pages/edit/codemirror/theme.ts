import { Compartment, Extension } from "@codemirror/state";
import { EditorView } from "@codemirror/view";
import { syntaxHighlighting } from "@codemirror/language";
import { classHighlighter } from "@lezer/highlight";

export interface ThemeConfig {
    fontSize: number;
    fontFamily: string;
    lineHeight: number;
    highlight: boolean;
}

export const defaultThemeConfig = (): ThemeConfig => ({
    fontSize: 16,
    fontFamily: "JetBrains Mono",
    lineHeight: 1.75,
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
            lineHeight: config.lineHeight,
        },
        ".cm-content": {
            padding: "1px 1rem",
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

export const highlightCategories: Record<string, string> = {
    "control-flow": "bg-opacity-30 bg-orange-300 text-orange-500",
    motion: "bg-opacity-30 bg-green-300 text-green-500",
    turn: "bg-opacity-30 bg-blue-300 text-blue-500",
    pen: "bg-opacity-30 bg-purple-300 text-purple-500",
};
