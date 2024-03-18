import { addDoc, collection, deleteDoc, doc, getDoc, getFirestore } from "firebase/firestore";
import { getUser } from "../helpers";
import { pureConverter } from "../helpers/database";
import { getFunctions, httpsCallable } from "firebase/functions";

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
    | PlaygroundPageEnergyItem
    | PlaygroundPageTextItem;

interface PlaygroundPageCodeItem {
    type: "code";
    code: string;
}

interface PlaygroundPageTurtleItem extends PlaygroundPageCodeItem {
    setup: "turtle";
    canvasWidth: number;
    canvasHeight: number;
}

interface PlaygroundPageMusicItem extends PlaygroundPageCodeItem {
    setup: "music";
}

interface PlaygroundPageMathItem extends PlaygroundPageCodeItem {
    setup: "math";
}

interface PlaygroundPageGameItem extends PlaygroundPageCodeItem {
    setup: "game";
    canvasScale: number;
}

interface PlaygroundPagePhysicsItem extends PlaygroundPageCodeItem {
    setup: "physics";
    canvasWidth: number;
    canvasHeight: number;
}

interface PlaygroundPageEnergyItem extends PlaygroundPageCodeItem {
    setup: "energy";
}

interface PlaygroundPageTextItem {
    type: "text";
    text: string;
}

const playgroundConverter = pureConverter<Playground>();

export type ListPlaygroundsFilter = "all" | "owned" | "shared";

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
    return result.data.playgrounds;
};

export const getPlayground = async (id: string) => {
    const firestore = getFirestore();

    const playground = await getDoc(
        doc(firestore, "playgrounds", id).withConverter(playgroundConverter),
    );

    return playground.data();
};

export const createPlayground = async () => {
    const user = await getUser();
    if (!user) {
        throw new Error("must be logged in to create a playground");
    }

    const firestore = getFirestore();

    const playground: Omit<Playground, "id"> = {
        owner: user.uid,
        collaborators: [],
        name: "Untitled",
        lastModified: new Date().toISOString(),
        pages: [],
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

export const deletePlayground = async (id: string) => {
    const firestore = getFirestore();
    await deleteDoc(doc(firestore, "playgrounds", id));
};
