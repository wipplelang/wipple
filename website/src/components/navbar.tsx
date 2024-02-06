import { createContext, useContext, useState } from "react";
import { useLocation } from "react-router-dom";
import { useStore } from "../store";
import Skeleton from "react-loading-skeleton";
import { Button } from ".";

export interface Navbar {
    primaryActions: JSX.Element | null;
    setPrimaryActions: (actions: JSX.Element | null) => void;
    secondaryActions: JSX.Element | null;
    setSecondaryActions: (actions: JSX.Element | null) => void;
}

const defaultNavbar: Navbar = {
    primaryActions: null,
    setPrimaryActions: () => {},
    secondaryActions: null,
    setSecondaryActions: () => {},
};

const NavbarContext = createContext(defaultNavbar);

export const NavbarProvider = (props: React.PropsWithChildren<{}>) => {
    const [primaryActions, setPrimaryActions] = useState<JSX.Element | null>(null);
    const [secondaryActions, setSecondaryActions] = useState<JSX.Element | null>(null);

    return (
        <NavbarContext.Provider
            value={{ primaryActions, setPrimaryActions, secondaryActions, setSecondaryActions }}
        >
            {props.children}
        </NavbarContext.Provider>
    );
};

export const useNavbar = () => useContext(NavbarContext);

export const Navbar = () => {
    const location = useLocation();
    const isHome = location.pathname === "/";

    const { primaryActions, secondaryActions } = useNavbar();

    const [store, _setStore] = useStore();

    return (
        <div
            className={`flex flex-row items-center justify-between px-4 h-20 ${
                isHome ? "bg-gray-50 dark:bg-gray-900" : "bg-white dark:bg-black"
            }`}
        >
            <div className="flex flex-row items-center gap-4">
                <img src="/images/logo.svg" alt="Wipple" className="w-10 h-10" />
                {primaryActions}
            </div>

            <div className="flex flex-row items-center gap-4">
                {secondaryActions}

                {store.user ? (
                    store.user.isAnonymous ? (
                        <Button role="primary" onClick={() => alert("TODO")}>
                            Sign In
                        </Button>
                    ) : (
                        <div>TODO: Waffle</div>
                    )
                ) : null}

                {store.user?.isAnonymous ?? false ? null : (
                    <div className="w-12 h-12 rounded-full overflow-clip">
                        {store.user ? (
                            store.user.photoURL ? (
                                <img
                                    src={store.user.photoURL}
                                    alt={store.user.displayName ?? ""}
                                    className="w-full h-full"
                                />
                            ) : (
                                <div className="w-full h-full bg-gray-300 dark:bg-gray-600"></div>
                            )
                        ) : (
                            <Skeleton circle className="w-full h-full" />
                        )}
                    </div>
                )}
            </div>
        </div>
    );
};
