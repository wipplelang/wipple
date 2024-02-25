import { EditorView, Decoration } from "@codemirror/view";
import { Compartment, Extension } from "@codemirror/state";
import { Diagnostic } from "../../../models";

export const diagnostics = new Compartment();

export const diagnosticsFromConfig = (config: { diagnostics: Diagnostic[] }): Extension => {
    const decorations = config.diagnostics.map((diagnostic) =>
        Decoration.mark({
            class: `underline underline-offset-4 decoration-line decoration-2 ${
                diagnostic.error ? "decoration-red-500" : "decoration-yellow-500"
            }`,
        }).range(diagnostic.primaryLabel.span.start, diagnostic.primaryLabel.span.end),
    );

    return EditorView.decorations.of(Decoration.set(decorations, true));
};
