import { useEffect, useState } from "react";

export interface AppProps {
    setOnMessage: (handler: (message: string, value: any) => Promise<void>) => void;
}

export const App = (props: AppProps) => {
    const [messages, setMessages] = useState<string[]>([]);
    const [background, setBackground] = useState<string>();

    useEffect(() => {
        props.setOnMessage(async (message, value) => {
            switch (message) {
                case "echo":
                    if (typeof value !== "string") {
                        throw new Error("'echo' expects a string");
                    }

                    setMessages((messages) => [...messages, value]);

                    break;
                case "background":
                    if (typeof value !== "string") {
                        throw new Error("'background' expects a string");
                    }

                    setBackground(value);

                    break;
                default:
                    throw new Error("unknown message");
            }
        });
    }, [props.setOnMessage]);

    return (
        <div style={{ background }}>
            {messages.map((message, index) => (
                <p key={index}>{message}</p>
            ))}
        </div>
    );
};
