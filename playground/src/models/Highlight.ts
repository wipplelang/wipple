import type colors from "@/assets/colors";

export interface Highlight {
    icon?: string;
    color: keyof typeof colors;
}
