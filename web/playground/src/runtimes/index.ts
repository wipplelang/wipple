import { PaletteCategory } from "../models";
import {
    Turtle,
    paletteCategories as turtlePaletteCategories,
    type Settings as TurtleSettings,
} from "./turtle";
import {
    Music,
    paletteCategories as musicPaletteCategories,
    type Settings as MusicSettings,
} from "./music";
import {
    Math,
    paletteCategories as mathPaletteCategories,
    type Settings as MathSettings,
} from "./math";
import {
    Game,
    paletteCategories as gamePaletteCategories,
    type Settings as GameSettings,
} from "./game";
import {
    Physics,
    paletteCategories as physicsPaletteCategories,
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
    game: {
        Component: Game,
        paletteCategories: gamePaletteCategories,
    },
    physics: {
        Component: Physics,
        paletteCategories: physicsPaletteCategories,
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

export type { TurtleSettings, MusicSettings, MathSettings, GameSettings, PhysicsSettings };
