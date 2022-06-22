import { useEffect } from "react";

export interface RunResult {
    annotations: Annotation[];
    output: [Span, string][];
    diagnostics: Diagnostic[];
}

export interface Annotation {
    span: Span;
    value: string;
}

export interface Span {
    start: number;
    end: number;
}

export interface Diagnostic {
    level: DiagnosticLevel;
    message: string;
    notes: Note[];
}

export type DiagnosticLevel = "Note" | "Warning" | "Error";

export interface Note {
    level: NoteLevel;
    span: Span;
    message: string;
}

export type NoteLevel = "Primary" | "Secondary";

let runner: Worker | undefined;

export const useRunner = () => {
    useEffect(() => {
        runner = new Worker(new URL("../runner/worker.js", import.meta.url));
    }, []);

    return {
        run: (code: string): Promise<RunResult> =>
            new Promise((resolve, reject) => {
                runner!.onmessage = (event) => resolve(event.data);
                runner!.onerror = (event) => reject(event.error);
                runner!.postMessage(code);
            }),
    };
};
