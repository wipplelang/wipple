import { EditorView, Decoration } from "@codemirror/view";
import { Compartment, Extension } from "@codemirror/state";
import { Diagnostic } from "../../../models";

export const diagnostics = new Compartment();

export const diagnosticsFromConfig = (config: { diagnostics: Diagnostic[] }): Extension =>
    EditorView.decorations.of((view) => {
        const decorations = config.diagnostics.flatMap((diagnostic) => {
            const { start, end } = diagnostic.primaryLabel.span;

            if (start === end || end > view.state.doc.length) {
                return [];
            }

            return [
                Decoration.mark({
                    class: `underline underline-offset-4 decoration-line decoration-2 ${
                        diagnostic.error ? "decoration-red-500" : "decoration-yellow-500"
                    }`,
                }).range(start, end),
            ];
        });

        return Decoration.set(decorations, true);
    });
