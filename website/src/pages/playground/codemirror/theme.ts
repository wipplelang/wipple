import { Compartment, Extension } from "@codemirror/state";
import { EditorView } from "@codemirror/view";
import { githubLight, githubDark } from "@uiw/codemirror-theme-github";

export interface ThemeConfig {
    dark: boolean;
    fontSize: number;
    fontFamily: string;
}

export const defaultThemeConfig = (): ThemeConfig => ({
    dark: window.matchMedia("(prefers-color-scheme: dark)").matches,
    fontSize: 16,
    fontFamily: "JetBrains Mono",
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
    config.dark ? githubDark : githubLight,
];

export const theme = new Compartment();
