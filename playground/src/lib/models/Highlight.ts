import type colors from "$lib/assets/colors";

export interface Highlight {
    icon?: string;
    color: keyof typeof colors;
}
