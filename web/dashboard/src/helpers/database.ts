import {
    doc,
    DocumentData,
    FirestoreDataConverter,
    getDoc,
    getFirestore,
} from "firebase/firestore";
import { produce } from "immer";
import { getUser } from "./auth";

interface Metadata {
    id: string;
}

const pureConverter = <T extends DocumentData>(): FirestoreDataConverter<T & Metadata, T> => ({
    fromFirestore: (snapshot, options) => ({
        id: snapshot.id,
        ...(snapshot.data(options) as any),
    }),
    toFirestore: (item: T & Metadata) =>
        produce(item, (item) => {
            delete (item as T).id;
        }),
});

export interface UserInfo {
    classroomCode?: string;
}

const userInfoConverter = pureConverter<UserInfo>();

export const getUserInfo = async () => {
    const user = await getUser();
    if (!user) {
        throw new Error("must be logged in to get user info");
    }

    const firestore = getFirestore();

    const playground = await getDoc(
        doc(firestore, "users", user.uid).withConverter(userInfoConverter),
    );

    return playground.data();
};
