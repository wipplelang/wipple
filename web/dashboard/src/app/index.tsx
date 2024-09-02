import { useEffect, useState } from "react";
import { produce } from "immer";
import { useStore } from "../store";
import { getUser } from "../helpers";
import { Dashboard } from "./dashboard";
import { CircularProgress } from "wipple-playground";

export const App = () => {
    const [store, setStore] = useStore();
    const [isLoading, setLoading] = useState(true);

    useEffect(() => {
        (async () => {
            try {
                const user = await getUser();

                if (user) {
                    setStore(
                        produce((store) => {
                            store.user = user;
                        }),
                    );
                }
            } catch (error) {
                console.error(error);
            } finally {
                setLoading(false);
            }
        })();
    }, [store.user]);

    return isLoading ? (
        <div className="flex items-center justify-center w-screen h-screen">
            <CircularProgress />
        </div>
    ) : (
        <div className="w-screen flex flex-col items-stretch">
            <Dashboard />
        </div>
    );
};
