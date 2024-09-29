import { PaletteCategory } from "../models";
import { Turtle, paletteCategories as turtlePaletteCategories } from "./turtle";
import { Music, paletteCategories as musicPaletteCategories } from "./music";
import { Math, paletteCategories as mathPaletteCategories } from "./math";

export interface Runtime {
    initialize: () => Promise<void>;
    onMessage: (message: string, value: any) => Promise<any>;
    cleanup: () => Promise<void>;
}

export type RuntimeComponent = React.ForwardRefExoticComponent<
    {
        id: string;
        isFullscreen: boolean;
        stopRunning: () => void;
    } & React.RefAttributes<Runtime>
>;

export const runtimes = {
    turtle: {
        Component: Turtle,
        paletteCategories: turtlePaletteCategories,
    },
    music: {
        Component: Music,
        paletteCategories: musicPaletteCategories,
    },
    math: {
        Component: Math,
        paletteCategories: mathPaletteCategories,
    },
};

export const defaultPaletteCategories: PaletteCategory[] = [
    {
        title: "Commands",
        items: [
            { title: "show", code: `show "Wipple"` },
            { title: "repeat", code: `repeat (2 times) {\n  _\n}`, replace: true },
        ],
    },
];
