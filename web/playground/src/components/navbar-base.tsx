import { createContext, useContext, useState } from "react";

export interface NavbarContext {
    primaryActions: JSX.Element | null;
    setPrimaryActions: (actions: JSX.Element | null) => void;
    secondaryActions: JSX.Element | null;
    setSecondaryActions: (actions: JSX.Element | null) => void;
}

const defaultNavbar: NavbarContext = {
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

export const NavbarBase = (props: {
    onClickHome: () => void;
    trailingActions: React.ReactNode;
}) => {
    const { primaryActions, secondaryActions } = useNavbar();

    return (
        <div className="flex flex-row items-center justify-between px-4 h-20">
            <div className="flex flex-row items-center gap-4">
                <button onClick={props.onClickHome}>
                    <img src="/playground/images/logo.svg" alt="Wipple" className="w-10 h-10" />
                </button>

                {primaryActions}
            </div>

            <div className="flex flex-row items-center gap-4">
                {secondaryActions}

                {props.trailingActions}
            </div>
        </div>
    );
};
