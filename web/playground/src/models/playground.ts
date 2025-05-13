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

export interface Playground {
    id: string;
    owner: string;
    name: string;
    lastModified: string;
    setup: PlaygroundSetup;
    code: string;
}

export const playgroundSetups = ["turtle", "music", "math"] as const;
export type PlaygroundSetup = null | (typeof playgroundSetups)[number];

const playgroundConverter = pureConverter<Playground>();

export const listPlaygrounds = async () => {
    const user = await getUser();
    if (!user) {
        return [];
    }

    const functions = getFunctions();
    const listPlaygrounds = httpsCallable<{}, { playgrounds: Playground[] }>(
        functions,
        "listPlaygrounds",
    );

    const result = await listPlaygrounds({});

    return result.data.playgrounds;
};

export const getPlayground = async (id: string) => {
    const user = await getUser();
    if (!user) {
        throw new Error("must be logged in to open a playground");
    }

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

export const createPlayground = async (setup: PlaygroundSetup) => {
    const user = await getUser();
    if (!user) {
        throw new Error("must be logged in to create a playground");
    }

    const firestore = getFirestore();

    const playground: Omit<Playground, "id"> = {
        owner: user.uid,
        name: "",
        lastModified: new Date().toISOString(),
        setup,
        code: "",
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
        name: `${playground.name} (Copy)`,
        lastModified: new Date().toISOString(),
        setup: playground.setup,
        code: playground.code,
    };

    const result = await addDoc(collection(firestore, "playgrounds"), newPlayground);
    return result.id;
};

export const deletePlayground = async (id: string) => {
    const firestore = getFirestore();
    await deleteDoc(doc(firestore, "playgrounds", id));
};
