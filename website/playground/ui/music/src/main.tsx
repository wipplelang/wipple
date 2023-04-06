import React from "react";
import ReactDOM from "react-dom/client";
import { App } from "./App";

export const onMessage: Record<string, (message: string, value: any) => Promise<any>> = {};

export const initialize = (id: string, container: HTMLElement) =>
    new Promise<void>((resolve) => {
        ReactDOM.createRoot(container).render(
            <React.StrictMode>
                <App
                    setOnMessage={(handler) => {
                        onMessage[id] = handler;
                        resolve();
                    }}
                />
            </React.StrictMode>
        );
    });
