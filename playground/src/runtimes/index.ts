import { PaletteItem } from "../models";
import {
    Turtle,
    paletteItems as turtlePaletteItems,
    assetItems as turtleAssetItems,
    type Settings as TurtleSettings,
} from "./turtle";
import {
    Music,
    paletteItems as musicPaletteItems,
    assetItems as musicAssetItems,
    type Settings as MusicSettings,
} from "./music";
import {
    Math,
    paletteItems as mathPaletteItems,
    assetItems as mathAssetItems,
    type Settings as MathSettings,
} from "./math";
import {
    Game,
    paletteItems as gamePaletteItems,
    assetItems as gameAssetItems,
    type Settings as GameSettings,
} from "./game";
import {
    Physics,
    paletteItems as physicsPaletteItems,
    assetItems as physicsAssetItems,
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
        assetItems: turtleAssetItems,
        paletteItems: turtlePaletteItems,
    },
    music: {
        Component: Music,
        assetItems: musicAssetItems,
        paletteItems: musicPaletteItems,
    },
    math: {
        Component: Math,
        assetItems: mathAssetItems,
        paletteItems: mathPaletteItems,
    },
    game: {
        Component: Game,
        assetItems: gameAssetItems,
        paletteItems: gamePaletteItems,
    },
    physics: {
        Component: Physics,
        assetItems: physicsAssetItems,
        paletteItems: physicsPaletteItems,
    },
};

export const defaultPaletteItems: PaletteItem[] = [
    { title: "show", code: `show "Wipple"` },
    { title: "repeat", code: `repeat (1 times) {\n  _\n}` },
];

export type { TurtleSettings, MusicSettings, MathSettings, GameSettings, PhysicsSettings };
