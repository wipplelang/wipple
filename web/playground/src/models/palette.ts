export interface PaletteCategory {
    title: string;
    items: PaletteItem[];
}

export interface PaletteItem {
    title: string;
    code: string;
    replace?: boolean;
}
