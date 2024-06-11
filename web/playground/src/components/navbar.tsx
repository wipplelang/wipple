import { createContext, useCallback, useContext, useState } from "react";
import { Link, useLocation } from "react-router-dom";
import { useStore } from "../store";
import { Button, Skeleton, Tooltip, useAlert } from ".";
import { signInWithGoogle, signOut } from "../helpers";
import { produce } from "immer";
import { MaterialSymbol } from "react-material-symbols";
import { User } from "firebase/auth";

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
    const isHome = /^\/playground\/?$/.test(location.pathname);

    const { primaryActions, secondaryActions } = useNavbar();
    const { displayAlert } = useAlert();

    const [store, setStore] = useStore();

    const handleSignIn = useCallback(() => {
        (async () => {
            try {
                const user = await signInWithGoogle();

                setStore(
                    produce((store) => {
                        store.user = user;
                    }),
                );
            } catch (error) {
                console.error(error);
            }
        })();
    }, []);

    const openUserSettings = useCallback(() => {
        displayAlert(UserSettings);
    }, [displayAlert]);

    return (
        <div
            className={`flex flex-row items-center justify-between px-4 h-20 ${
                isHome ? "bg-gray-50 dark:bg-gray-900" : ""
            }`}
        >
            <div className="flex flex-row items-center gap-4">
                <Link to={import.meta.env.BASE_URL}>
                    <img src="/playground/images/logo.svg" alt="Wipple" className="w-10 h-10" />
                </Link>

                {primaryActions}
            </div>

            <div className="flex flex-row items-center gap-4">
                {secondaryActions}

                {store.offline ?? true ? (
                    <Tooltip description="You're offline.">
                        <MaterialSymbol
                            icon="cloud_off"
                            className="text-3xl text-gray-400 dark:text-gray-600"
                        />
                    </Tooltip>
                ) : null}

                {store.user?.isAnonymous ?? false ? (
                    <Button role="primary" onClick={handleSignIn}>
                        Sign In
                    </Button>
                ) : (
                    <button
                        onClick={openUserSettings}
                        className="flex flex-row items-center gap-4 -mx-2 -my-1 px-2 p-1 rounded-lg transition hover:bg-gray-200 dark:hover:bg-gray-800"
                    >
                        {store.user ? (
                            <MaterialSymbol icon="apps" className="text-3xl text-gray-500" />
                        ) : null}

                        <div className="w-12 h-12 rounded-full overflow-clip">
                            {store.user ? (
                                <UserPhoto user={store.user} />
                            ) : (
                                <Skeleton circle className="w-full h-full" />
                            )}
                        </div>
                    </button>
                )}
            </div>
        </div>
    );
};

const UserPhoto = (props: { user: User }) =>
    props.user.photoURL ? (
        <img
            src={props.user.photoURL}
            alt={props.user.displayName ?? ""}
            className="w-full h-full"
        />
    ) : (
        <div className="w-full h-full bg-gray-300 dark:bg-gray-600"></div>
    );

const UserSettings = (props: { dismiss: () => void }) => {
    const [store, setStore] = useStore();

    const [user, _setUser] = useState(store.user!);

    const handleSignOut = useCallback(() => {
        (async () => {
            try {
                await signOut();

                setStore(
                    produce((store) => {
                        store.user = undefined;
                    }),
                );

                props.dismiss();
            } catch (error) {
                console.error(error);
            }
        })();
    }, []);

    return (
        <div className="flex flex-col items-stretch gap-4 w-[300px]">
            <div className="flex flex-col items-center gap-4">
                <div className="w-20 h-20 rounded-full overflow-clip">
                    <UserPhoto user={user} />
                </div>

                {user.displayName ? (
                    <p className="text-2xl font-semibold">{user.displayName}</p>
                ) : null}
            </div>

            <div className="flex flex-col items-stretch gap-2.5">
                <Button role="destructive" onClick={handleSignOut}>
                    Sign Out
                </Button>

                <Button role="secondary" fill={false} onClick={props.dismiss}>
                    Cancel
                </Button>
            </div>
        </div>
    );
};
