import { PaletteItem } from "../models";
import {
    Turtle,
    paletteItems as turtlePaletteItems,
    type Settings as TurtleSettings,
} from "./turtle";
import { Music, paletteItems as musicPaletteItems, type Settings as MusicSettings } from "./music";
import { Math, paletteItems as mathPaletteItems, type Settings as MathSettings } from "./math";

export interface Runtime {
    initialize: () => Promise<void>;
    onMessage: (message: string, value: any) => Promise<any>;
    cleanup: () => Promise<void>;
}

export type RuntimeComponent<Settings> = React.ForwardRefExoticComponent<
    {
        id: string;
        settings: Settings | undefined;
        onChangeSettings: (settings: Settings) => void;
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
    math: {
        Component: Math,
        paletteItems: mathPaletteItems,
    },
};

export const defaultPaletteItems: PaletteItem[] = [
    { title: "show", code: `show "Wipple"` },
    { title: "repeat", code: `repeat (1 times) {\n  _\n}` },
];

export type { TurtleSettings, MusicSettings, MathSettings };
