import type { RuntimeComponent } from ".";
import { forwardRef, useImperativeHandle, useState } from "react";
import { produce } from "immer";

export const Turtle: RuntimeComponent = forwardRef((props, ref) => {
    const [messages, setMessages] = useState<[string, any][]>([]);

    useImperativeHandle(ref, () => ({
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
            <p>
                <em>ID: {props.id}</em>
            </p>

            {messages.map(([message, value], index) => (
                <div key={index}>
                    <p>
                        <strong>{message}</strong>:
                        <br />
                        <pre>{JSON.stringify(value)}</pre>
                    </p>
                </div>
            ))}
        </div>
    );
});
