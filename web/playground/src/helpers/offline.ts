import { getDatabase, ref, onValue } from "firebase/database";
import { useEffect, useState } from "react";

export const useIsOffline = () => {
    const [offline, setOffline] = useState<boolean>();

    useEffect(() => {
        const db = getDatabase();
        const connectedRef = ref(db, ".info/connected");

        const unsubscribe = onValue(connectedRef, (snapshot) => {
            const connected = snapshot.val() === true;
            console.log("Connected:", connected);
            setOffline(!connected);
        });

        return () => {
            unsubscribe();
        };
    }, []);

    return offline;
};
