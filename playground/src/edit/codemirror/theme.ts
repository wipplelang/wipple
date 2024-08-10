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
    fontFamily: "JetBrains Mono Variable",
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
            padding: "0 1rem",
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
    io: "bg-opacity-30 bg-yellow-300 text-yellow-500",
    unit: "bg-opacity-30 bg-sky-300 text-sky-500",
    "turtle-motion": "bg-opacity-30 bg-green-300 text-green-500",
    "turtle-turn": "bg-opacity-30 bg-blue-300 text-blue-500",
    "turtle-pen": "bg-opacity-30 bg-purple-300 text-purple-500",
    "music-play": "bg-opacity-30 bg-green-300 text-green-500",
    "music-instrument": "bg-opacity-30 bg-purple-300 text-purple-500",
    "math-plot": "bg-opacity-30 bg-green-300 text-green-500",
    "math-color": "bg-opacity-30 bg-purple-300 text-purple-500",
    "physics-motion": "bg-opacity-30 bg-green-300 text-green-500",
    "physics-utility": "bg-opacity-30 bg-orange-300 text-orange-500",
    "physics-measure": "bg-opacity-30 bg-yellow-300 text-yellow-500",
};
