import { EditorView, Decoration } from "@codemirror/view";
import { Compartment, Extension, Prec } from "@codemirror/state";
import { HighlightedCode } from "./index";

export const diagnostics = new Compartment();

export const highlightedCodeFromConfig = (config: {
    highlightedCode: HighlightedCode | undefined;
}): Extension =>
    Prec.lowest(
        EditorView.decorations.of((view) => {
            if (!config.highlightedCode) {
                return Decoration.none;
            }

            const { startIndex, endIndex } = config.highlightedCode;

            if (startIndex === endIndex || endIndex > view.state.doc.length) {
                return Decoration.none;
            }

            return Decoration.set([
                Decoration.mark({
                    class: `outline outline-2 rounded-md ${
                        config.highlightedCode.severity === "error"
                            ? "outline-red-300 dark:outline-red-600 bg-red-500 bg-opacity-10"
                            : "outline-yellow-300 dark:outline-yellow-600 bg-yellow-500 bg-opacity-10"
                    }`,
                }).range(startIndex, endIndex),
            ]);
        }),
    );
