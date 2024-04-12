import * as admin from "firebase-admin";
import { onCall } from "firebase-functions/v2/https";

admin.initializeApp();

interface ListPlaygroundsRequest {
    filter: "all" | "owned" | "shared";
}

export const listPlaygrounds = onCall<ListPlaygroundsRequest>(async (request) => {
    if (!request.auth) {
        return { playgrounds: [] };
    }

    const collection = admin.firestore().collection("playgrounds");

    let query: admin.firestore.Query;
    switch (request.data.filter) {
        case "all":
            query = collection.where(
                admin.firestore.Filter.or(
                    admin.firestore.Filter.where("owner", "==", request.auth.uid),
                    admin.firestore.Filter.where(
                        "collaborators",
                        "array-contains",
                        request.auth.uid,
                    ),
                ),
            );
            break;
        case "owned":
            query = collection.where("owner", "==", request.auth.uid);
            break;
        case "shared":
            query = collection.where("collaborators", "array-contains", request.auth.uid);
            break;
    }

    const result = await query.select("owner", "name", "lastModified").get();

    const playgrounds = result.docs.map((doc): any => ({
        id: doc.id,
        ...doc.data(),
    }));

    playgrounds.sort(
        (a, b) => new Date(b.lastModified).valueOf() - new Date(a.lastModified).valueOf(),
    );

    return { playgrounds };
});
