import { useEffect, useState } from "react";
import { produce } from "immer";

export interface AppProps {
    setOnMessage: (handler: (message: string, value: any) => Promise<any>) => void;
}

export const App = (props: AppProps) => {
    useEffect(() => {
        (async () => {
            props.setOnMessage(async (message, value) => {
                try {
                    switch (message) {
                        case "todo":
                            console.log("TODO:", value);
                            break;
                        default:
                            throw new Error("unknown message");
                    }
                } catch (error) {
                    console.error("[graphing] error:", error);
                }
            });
        })();
    }, []);

    return <p>TODO</p>;
};
