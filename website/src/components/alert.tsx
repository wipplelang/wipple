import { createContext, useContext, useState } from "react";
import { Transition, defaultAnimationDuration } from ".";

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
                    requestAnimationFrame(() => {
                        setContents(() => contents);
                    });
                },
            }}
        >
            {props.children}

            <div
                className={`absolute inset-0 transition ${
                    Contents ? "bg-black bg-opacity-10 backdrop-blur" : "pointer-events-none"
                }`}
                style={{ zIndex: 9999 }}
            >
                <div className="flex items-center justify-center w-screen h-screen p-4">
                    <Transition
                        value={Contents}
                        exitAnimationDuration={defaultAnimationDuration}
                        inClassName="animate-in slide-in-from-bottom fade-in-10"
                        outClassName="animate-out slide-out-to-bottom fade-out-10"
                    >
                        {(Contents) => (
                            <div className="bg-white dark:bg-gray-900 rounded-lg p-4">
                                <Contents dismiss={() => setContents(undefined)} />
                            </div>
                        )}
                    </Transition>
                </div>
            </div>
        </AlertContext.Provider>
    );
};

export const useAlert = () => useContext(AlertContext);
