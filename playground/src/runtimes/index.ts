import { PaletteItem } from "../models";
import {
    Turtle,
    paletteItems as turtlePaletteItems,
    type Settings as TurtleSettings,
} from "./turtle";
import { Music, paletteItems as musicPaletteItems, type Settings as MusicSettings } from "./music";
import { Math, paletteItems as mathPaletteItems, type Settings as MathSettings } from "./math";
import { Game, paletteItems as gamePaletteItems, type Settings as GameSettings } from "./game";
import {
    Physics,
    paletteItems as physicsPaletteItems,
    type Settings as PhysicsSettings,
} from "./physics";

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
    game: {
        Component: Game,
        paletteItems: gamePaletteItems,
    },
    physics: {
        Component: Physics,
        paletteItems: physicsPaletteItems,
    },
};

export const defaultPaletteItems: PaletteItem[] = [
    { title: "show", code: `show "Wipple"` },
    { title: "repeat", code: `repeat (1 times) {\n  _\n}`, replace: true },
];

export type { TurtleSettings, MusicSettings, MathSettings, GameSettings, PhysicsSettings };
