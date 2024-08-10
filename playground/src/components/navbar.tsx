import { createContext, useContext, useState } from "react";
import { Tooltip } from ".";
import { MaterialSymbol } from "react-material-symbols";

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

export const Navbar = (props: {
    isHome: boolean;
    onClickHome: () => void;
    offline: boolean;
    trailingActions: JSX.Element;
}) => {
    const { primaryActions, secondaryActions } = useNavbar();

    return (
        <div
            className={`flex flex-row items-center justify-between px-4 h-20 ${
                props.isHome ? "bg-gray-50 dark:bg-gray-900" : ""
            }`}
        >
            <div className="flex flex-row items-center gap-4">
                <button onClick={props.onClickHome}>
                    <img src="/playground/images/logo.svg" alt="Wipple" className="w-10 h-10" />
                </button>

                {primaryActions}
            </div>

            <div className="flex flex-row items-center gap-4">
                {secondaryActions}

                {props.offline ?? true ? (
                    <Tooltip description="You're offline.">
                        <MaterialSymbol
                            icon="cloud_off"
                            className="text-3xl text-gray-400 dark:text-gray-600"
                        />
                    </Tooltip>
                ) : null}

                {props.trailingActions}
            </div>
        </div>
    );
};
