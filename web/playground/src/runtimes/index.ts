import { PaletteItem } from "../models";
import { Turtle, paletteItems as turtlePaletteItems } from "./turtle";
import { Music, paletteItems as musicPaletteItems } from "./music";

export interface Runtime {
    initialize: () => Promise<void>;
    onMessage: (message: string, value: any) => Promise<any>;
    cleanup: () => Promise<void>;
}

export type RuntimeComponent = React.ForwardRefExoticComponent<
    {
        id: string;
        call: (func: any, ...inputs: any[]) => Promise<any>;
        stopRunning: () => void;
    } & React.RefAttributes<Runtime>
>;

export const runtimes = {
    turtle: {
        Component: Turtle,
        paletteItems: turtlePaletteItems,
    },
    music: {
        Component: Music,
        paletteItems: musicPaletteItems,
    },
};

export const defaultPaletteItems: PaletteItem[] = [
    { title: "show", code: `show "Wipple"` },
    { title: "repeat", code: `repeat (1 times) {\n  _\n}` },
];
