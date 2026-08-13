export type Groups = {
    labels?: string[];
    locations: { start: number; end: number; primary?: boolean }[];
}[];
