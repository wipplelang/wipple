import { PaletteItem } from "../models";
import { Turtle, paletteItems as turtlePaletteItems } from "./turtle";

export interface Runtime {
    initialize: () => Promise<void>;
    onMessage: (message: string, value: any) => Promise<any>;
    cleanup: () => Promise<void>;
}

export type RuntimeComponent = React.ForwardRefExoticComponent<
    { id: string } & React.RefAttributes<Runtime>
>;

export const runtimes = {
    turtle: {
        Component: Turtle,
        paletteItems: turtlePaletteItems,
    },
};

export const defaultPaletteItems: PaletteItem[] = [
    { title: "show", code: `show "Wipple"` },
    { title: "repeat", code: `repeat (1 times) {\n  _\n}` },
];
