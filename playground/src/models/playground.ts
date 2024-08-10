import {
    MathSettings,
    MusicSettings,
    TurtleSettings,
    GameSettings,
    PhysicsSettings,
} from "../runtimes";

export interface PlaygroundListItem {
    id: string;
    owner: string;
    name: string;
    lastModified: string;
}

export interface Playground {
    id: string;
    owner: string;
    collaborators: string[];
    name: string;
    lastModified: string;
    locked?: boolean;
    pages: PlaygroundPage[];
}

export interface PlaygroundPage {
    id: string;
    name: string;
    items: PlaygroundPageItem[];
}

export type PlaygroundPageItem =
    | PlaygroundPageCodeItem
    | PlaygroundPageTurtleItem
    | PlaygroundPageMusicItem
    | PlaygroundPageMathItem
    | PlaygroundPageGameItem
    | PlaygroundPagePhysicsItem
    | PlaygroundPageTextItem;

interface PlaygroundPageCodeItem {
    type: "code";
    code: string;
    originalCode?: string;
}

type RuntimeItem<Name extends string, Settings> = PlaygroundPageCodeItem & {
    setup: Name;
    settings?: Settings;
};

type PlaygroundPageTurtleItem = RuntimeItem<"turtle", TurtleSettings>;
type PlaygroundPageMusicItem = RuntimeItem<"music", MusicSettings>;
type PlaygroundPageMathItem = RuntimeItem<"math", MathSettings>;
type PlaygroundPageGameItem = RuntimeItem<"game", GameSettings>;
type PlaygroundPagePhysicsItem = RuntimeItem<"physics", PhysicsSettings>;

interface PlaygroundPageTextItem {
    type: "text";
    text: string;
    locked: boolean;
}
