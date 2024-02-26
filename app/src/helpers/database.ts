import { DocumentData, FirestoreDataConverter } from "firebase/firestore";
import { produce } from "immer";

interface Metadata {
    id: string;
}

export const pureConverter = <T extends DocumentData>(): FirestoreDataConverter<
    T & Metadata,
    T
> => ({
    fromFirestore: (snapshot, options) => ({
        id: snapshot.id,
        ...(snapshot.data(options) as any),
    }),
    toFirestore: (item: T & Metadata) =>
        produce(item, (item) => {
            delete (item as T).id;
        }),
});
