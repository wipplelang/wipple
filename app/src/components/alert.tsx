import { createContext, useContext, useState } from "react";
import { Transition } from ".";

interface AlertContentsProps {
    dismiss: () => void;
}

export interface Alert {
    isDisplayingAlert: boolean;
    displayAlert: (contents: (props: AlertContentsProps) => JSX.Element) => void;
}

const defaultAlert: Alert = {
    isDisplayingAlert: false,
    displayAlert: () => {},
};

const AlertContext = createContext(defaultAlert);

export const AlertProvider = (props: React.PropsWithChildren<{}>) => {
    const [Contents, setContents] = useState<(props: AlertContentsProps) => JSX.Element>();

    return (
        <AlertContext.Provider
            value={{
                isDisplayingAlert: Contents != null,
                displayAlert: (contents) => {
                    setContents(() => contents);
                },
            }}
        >
            {props.children}

            <div
                className={`absolute inset-0 transition ${
                    Contents ? "bg-black bg-opacity-10 backdrop-blur" : "pointer-events-none"
                } z-10`}
            >
                <div className="flex items-center justify-center w-screen h-screen p-4">
                    <Transition
                        in={Contents != null}
                        inStyle={{ opacity: 1, y: 0 }}
                        outStyle={{ opacity: 0, y: "100%" }}
                        dynamicChildren
                    >
                        {Contents ? (
                            <div className="max-w-xl p-4">
                                <div className="bg-white dark:bg-gray-900 rounded-lg p-4">
                                    <Contents dismiss={() => setContents(undefined)} />
                                </div>{" "}
                            </div>
                        ) : null}
                    </Transition>
                </div>
            </div>
        </AlertContext.Provider>
    );
};

export const useAlert = () => useContext(AlertContext);
