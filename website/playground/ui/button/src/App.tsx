import { useEffect, useState } from "react";

export interface AppProps {
    setOnMessage: (handler: (message: string, value: any) => Promise<void>) => void;
}

export const App = (props: AppProps) => {
    const [title, setTitle] = useState("Button");
    const [action, setAction] = useState<(input: null) => void>();

    useEffect(() => {
        props.setOnMessage(async (message, value) => {
            switch (message) {
                case "title":
                    if (typeof value !== "string") {
                        throw new Error("'title' expects a string");
                    }

                    setTitle(value);

                    break;
                case "action":
                    if (typeof value !== "function") {
                        throw new Error("'action' expects a function");
                    }

                    setAction(() => value);

                    break;
                default:
                    throw new Error("unknown message");
            }
        });
    }, [props.setOnMessage]);

    return <button onClick={() => action?.(null)}>{title}</button>;
};
