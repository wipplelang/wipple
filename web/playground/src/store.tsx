import { User } from "firebase/auth";
import { createContext, useContext, useState } from "react";
import { TutorialStep } from "./models";
import { UserInfo } from "./models";

export interface Store {
    offline?: boolean;
    user?: User;
    userInfo?: UserInfo;
    activeTutorialStep?: TutorialStep;
    isPrinting?: boolean;
}

const defaultStore: Store = {};

const StoreContext = createContext<[Store, React.Dispatch<React.SetStateAction<Store>>]>([
    defaultStore,
    () => {},
]);

export const StoreProvider = (props: React.PropsWithChildren<{}>) => {
    const [store, setStore] = useState(defaultStore);

    return (
        <StoreContext.Provider value={[store, setStore]}>{props.children}</StoreContext.Provider>
    );
};

export const useStore = () => useContext(StoreContext);
