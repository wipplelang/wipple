import * as admin from "firebase-admin";
import { onCall } from "firebase-functions/v2/https";

admin.initializeApp();

interface ListPlaygroundsRequest {}

export const listPlaygrounds = onCall<ListPlaygroundsRequest>(async (request) => {
    if (!request.auth) {
        return { playgrounds: [] };
    }

    const result = await admin
        .firestore()
        .collection("playgrounds")
        .where("owner", "==", request.auth.uid)
        .get();

    const playgrounds = result.docs.map((doc): any => ({
        id: doc.id,
        ...doc.data(),
    }));

    playgrounds.sort(
        (a, b) => new Date(b.lastModified).valueOf() - new Date(a.lastModified).valueOf(),
    );

    return { playgrounds };
});
