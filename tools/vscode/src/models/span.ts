export interface Span {
    path: FilePath;
    start: number;
    end: number;
}

export type FilePath =
    | { type: "Path"; value: string }
    | { type: "Url"; value: string }
    | { type: "Virtual"; value: string }
    | { type: "Builtin"; value: string };
