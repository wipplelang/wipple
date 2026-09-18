export type Groups = Record<
    string,
    {
        labels?: string[];
        locations: { start: number; end: number; primary?: boolean }[];
    }
>;
