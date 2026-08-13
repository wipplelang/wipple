export interface Command {
    documentationName?: string | null;
    code: string;
    surround?: { before: string; after: string };
}
