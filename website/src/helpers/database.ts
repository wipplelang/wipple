import { DocumentData, FirestoreDataConverter } from "firebase/firestore";
import { produce } from "immer";

export const pureConverter = <T extends DocumentData & { id: string }>(): FirestoreDataConverter<
    T,
    Omit<T, "id">
> => ({
    fromFirestore: (snapshot, options) => ({
        id: snapshot.id,
        ...(snapshot.data(options) as any),
    }),
    toFirestore: (item: T) =>
        produce(item, (item) => {
            delete (item as Omit<T, "id">).id;
        }),
});
