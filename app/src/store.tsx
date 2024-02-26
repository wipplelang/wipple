import { User } from "firebase/auth";
import { createContext, useContext, useState } from "react";

export interface Store {
    user?: User;
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
