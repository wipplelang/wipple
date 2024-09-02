import { useCallback, useState } from "react";
import { useLocation, useNavigate } from "react-router-dom";
import { signIn, signOut } from "../helpers";
import { produce } from "immer";
import { MaterialSymbol } from "react-material-symbols";
import { User } from "firebase/auth";
import { Navbar as NavbarBase, Button, useAlert } from "wipple-playground";
import { useStore } from "../store";
import { updateUserInfo } from "../models";

export const Navbar = () => {
    const [store, _updateStore] = useStore();

    const { displayAlert } = useAlert();

    const handleSignIn = useCallback(async () => {
        await signIn();
    }, []);

    const openUserSettings = useCallback(() => {
        displayAlert(UserSettings);
    }, [displayAlert]);

    const navigate = useNavigate();

    const location = useLocation();
    const isHome = /^\/playground\/?$/.test(location.pathname);

    return (
        <NavbarBase
            offline={store.offline ?? true}
            trailingActions={
                store.user ? (
                    store.user.isAnonymous ? (
                        <Button role="primary" onClick={handleSignIn}>
                            Sign In
                        </Button>
                    ) : (
                        <button
                            onClick={openUserSettings}
                            className="flex flex-row items-center gap-4 -mx-2 -my-1 px-2 p-1 rounded-lg transition hover:bg-gray-200 dark:hover:bg-gray-800"
                        >
                            <MaterialSymbol icon="apps" className="text-3xl text-gray-500" />

                            <div className="w-12 h-12 rounded-full overflow-clip">
                                <UserPhoto user={store.user} />
                            </div>
                        </button>
                    )
                ) : (
                    <></>
                )
            }
            isHome={isHome}
            onClickHome={() => navigate(import.meta.env.BASE_URL)}
        />
    );
};

const UserPhoto = (props: { user: User }) =>
    props.user.photoURL ? (
        <img
            src={props.user.photoURL}
            alt={props.user.displayName ?? ""}
            crossOrigin="anonymous"
            className="w-full h-full"
        />
    ) : (
        <div className="w-full h-full bg-gray-300 dark:bg-gray-600"></div>
    );

const UserSettings = (props: { dismiss: () => void }) => {
    const [store, setStore] = useStore();

    const [user, _setUser] = useState(store.user!);

    const joinClass = useCallback(() => {
        (async () => {
            const code = prompt("Enter your class code:", store.userInfo?.classroomCode);
            if (!code) {
                return;
            }

            const newUserInfo = {
                ...(store.userInfo ?? {}),
                classroomCode: code,
            };

            try {
                await updateUserInfo(newUserInfo);

                setStore(
                    produce((store) => {
                        store.userInfo = newUserInfo;
                    }),
                );
            } catch (error) {
                console.error(error);
            }
        })();
    }, [store.userInfo]);

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

            <div className="flex flex-col items-stretch gap2.5">
                <Button role="primary" onClick={joinClass}>
                    Join Class
                </Button>
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
