import { EditorView, Decoration } from "@codemirror/view";
import { Compartment, Extension } from "@codemirror/state";
import { RenderedDiagnostic } from "wipple-render";

export const diagnostics = new Compartment();

export const diagnosticsFromConfig = (config: { diagnostics: RenderedDiagnostic[] }): Extension =>
    EditorView.decorations.of((view) => {
        const decorations = config.diagnostics.flatMap((diagnostic) => {
            const { start, end } = diagnostic.location;

            if (start.index === end.index || end.index > view.state.doc.length) {
                return [];
            }

            return [
                Decoration.mark({
                    class: `underline underline-offset-4 decoration-line decoration-2 ${
                        diagnostic.severity === "error"
                            ? "decoration-red-500"
                            : "decoration-yellow-500"
                    }`,
                }).range(start.index, end.index),
            ];
        });

        return Decoration.set(decorations, true);
    });
