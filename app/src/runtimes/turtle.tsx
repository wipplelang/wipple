import type { RuntimeComponent } from ".";
import { forwardRef, useImperativeHandle, useState } from "react";
import { produce } from "immer";

export const Turtle: RuntimeComponent = forwardRef((_props, ref) => {
    const [messages, setMessages] = useState<[string, any][]>([]);

    useImperativeHandle(ref, () => ({
        reset: async () => {
            setMessages([]);
        },
        onMessage: async (message, value) => {
            setMessages(
                produce((messages) => {
                    messages.push([message, value]);
                }),
            );

            return value; // echo
        },
    }));

    return (
        <div className="flex flex-col">
            {messages.map(([message, value], index) => (
                <div key={index}>
                    <p>
                        <strong>{message}</strong>: <code>{JSON.stringify(value)}</code>
                    </p>
                </div>
            ))}
        </div>
    );
});
