export * from "./edit";
export * from "./home";

import { useEffect } from "react";
import { Outlet, ScrollRestoration, useLocation, useNavigate } from "react-router-dom";
import { produce } from "immer";
import { useStore } from "../store";
import { getUser } from "../helpers";
import { MaterialSymbol } from "react-material-symbols";
import { CircularProgress } from "@mui/material";
import { Transition } from "../components";

export const RootPage = () => {
    const [store, setStore] = useStore();

    const location = useLocation();
    const navigate = useNavigate();

    useEffect(() => {
        (async () => {
            try {
                const user = await getUser();

                setStore(
                    produce((store) => {
                        store.isLoading = false;
                    }),
                );

                if (user) {
                    setStore(
                        produce((store) => {
                            store.user = user;
                        }),
                    );
                } else if (location.pathname !== import.meta.env.BASE_URL) {
                    alert("You must be logged in to view this page.");
                    navigate(import.meta.env.BASE_URL);
                }
            } catch (error) {
                console.error(error);
            }
        })();
    }, [store.user]);

    useEffect(() => {
        if (store.isPrinting) {
            document.body.classList.add("no-dark-mode");
        } else {
            document.body.classList.remove("no-dark-mode");
        }
    }, [store.isPrinting]);

    return store.isLoading ? (
        <div className="flex items-center justify-center w-screen h-screen">
            <Transition
                in
                animateOnMount
                inStyle={{ opacity: 1 }}
                outStyle={{ opacity: 0 }}
                delay={500}
            >
                <CircularProgress />
            </Transition>
        </div>
    ) : (
        <>
            <div className="w-screen flex flex-col items-stretch">
                <Outlet />
                <ScrollRestoration />
            </div>

            {store.isPrinting ? <PrintingOverlay /> : null}
        </>
    );
};

const PrintingOverlay = () => (
    <div className="sticky inset-0 w-screen h-screen flex flex-col items-center justify-center gap-2 text-2xl bg-gray-600 text-white">
        <MaterialSymbol icon="print" className="text-5xl" />
        <p>Printing...</p>
    </div>
);
