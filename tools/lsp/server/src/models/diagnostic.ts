import { Span } from "./span";

export interface Diagnostic {
    level: DiagnosticLevel;
    message: string;
    note: Note[];
}

export type DiagnosticLevel = "Note" | "Warning" | "Error";

export interface Note {
    level: NoteLevel;
    span: Span;
    message: string;
}

export type NoteLevel = "Primary" | "Secondary";
