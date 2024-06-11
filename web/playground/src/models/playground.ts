import {
    addDoc,
    collection,
    deleteDoc,
    doc,
    getDoc,
    getFirestore,
    setDoc,
} from "firebase/firestore";
import { getUser } from "../helpers";
import { pureConverter } from "../helpers/database";
import { getFunctions, httpsCallable } from "firebase/functions";
import { nanoid } from "nanoid";
import {
    MathSettings,
    MusicSettings,
    TurtleSettings,
    GameSettings,
    PhysicsSettings,
} from "../runtimes";
import { Lesson } from "../lessons";
import Dexie from "dexie";

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

const playgroundConverter = pureConverter<Playground>();

const cache = new Dexie("wipple-playground-cache");
cache.version(1).stores({
    "all-playgrounds": "&id",
    "owned-playgrounds": "&id",
    "shared-playgrounds": "&id",
});

export type ListPlaygroundsFilter = "all" | "owned" | "shared";

export const listCachedPlaygrounds = async (options: { filter: ListPlaygroundsFilter }) => {
    const user = await getUser();
    if (!user) {
        return [];
    }

    try {
        await cache.open();

        const playgrounds = await cache
            .table<PlaygroundListItem>(`${options.filter}-playgrounds`)
            .toArray();

        playgrounds.sort(
            (a, b) => new Date(b.lastModified).getTime() - new Date(a.lastModified).getTime(),
        );

        return playgrounds;
    } catch (error) {
        console.error(error);

        return [];
    }
};

export const listPlaygrounds = async (options: { filter: ListPlaygroundsFilter }) => {
    const user = await getUser();
    if (!user) {
        return [];
    }

    const functions = getFunctions();
    const listPlaygrounds = httpsCallable<
        { filter: ListPlaygroundsFilter },
        { playgrounds: PlaygroundListItem[] }
    >(functions, "listPlaygrounds");

    const result = await listPlaygrounds({ filter: options.filter });
    const playgrounds = result.data.playgrounds;

    await cache.open();
    const table = cache.table<PlaygroundListItem>(`${options.filter}-playgrounds`);
    await cache.transaction("rw!", table, async () => {
        table.clear();
        table.bulkAdd(playgrounds);
    });

    return playgrounds;
};

export const getPlayground = async (id: string) => {
    const firestore = getFirestore();

    const playground = await getDoc(
        doc(firestore, "playgrounds", id).withConverter(playgroundConverter),
    );

    return playground.data();
};

export const updatePlayground = async (playground: Playground) => {
    const firestore = getFirestore();
    await setDoc(doc(collection(firestore, "playgrounds"), playground.id), playground);
};

export const createPlayground = async ({
    name = "Untitled",
    pageName = "Untitled",
    initialItems = [],
}: {
    name?: string;
    pageName?: string;
    initialItems?: PlaygroundPageItem[];
} = {}) => {
    const user = await getUser();
    if (!user) {
        throw new Error("must be logged in to create a playground");
    }

    const firestore = getFirestore();

    const playground: Omit<Playground, "id"> = {
        owner: user.uid,
        collaborators: [],
        name,
        lastModified: new Date().toISOString(),
        pages: [
            {
                id: nanoid(20),
                name: pageName,
                items: initialItems,
            },
        ],
    };

    const result = await addDoc(collection(firestore, "playgrounds"), playground);
    return result.id;
};

export const duplicatePlayground = async (id: string) => {
    const user = await getUser();
    if (!user) {
        throw new Error("must be logged in to duplicate a playground");
    }

    const firestore = getFirestore();

    const playground = await getPlayground(id);
    if (!playground) {
        throw new Error(`no such playground ${id}`);
    }

    const newPlayground: Omit<Playground, "id"> = {
        owner: user.uid,
        collaborators: [], // TODO: Allow user to share copy with the same people
        name: `${playground.name} (Copy)`,
        lastModified: new Date().toISOString(),
        pages: playground.pages,
    };

    const result = await addDoc(collection(firestore, "playgrounds"), newPlayground);
    return result.id;
};

export const createLesson = async (lesson: Lesson) => {
    const user = await getUser();
    if (!user) {
        throw new Error("must be logged in to duplicate a playground");
    }

    const firestore = getFirestore();

    const playground: Omit<Playground, "id"> = {
        owner: user.uid,
        collaborators: [],
        name: lesson.name,
        lastModified: new Date().toISOString(),
        pages: lesson.pages,
        locked: true,
    };

    const result = await addDoc(collection(firestore, "playgrounds"), playground);
    return result.id;
};

export const deletePlayground = async (id: string) => {
    const firestore = getFirestore();
    await deleteDoc(doc(firestore, "playgrounds", id));
};
