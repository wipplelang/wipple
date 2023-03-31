import React from "react";
import ReactDOM from "react-dom/client";
import { App } from "./App";

export let onMessage: (message: string, value: any) => Promise<void>;

export const initialize = (container: HTMLElement) =>
    new Promise<void>((resolve) => {
        ReactDOM.createRoot(container).render(
            <React.StrictMode>
                <App
                    setOnMessage={(handler) => {
                        onMessage = handler;
                        resolve();
                    }}
                />
            </React.StrictMode>
        );
    });
