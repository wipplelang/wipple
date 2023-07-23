import React from "react";
import ReactDOM from "react-dom/client";
import { App } from "./App";
import "../common";

declare global {
    var __wipple_program: any;
}

ReactDOM.createRoot(document.getElementById("root")!).render(
    <React.StrictMode>
        <App program={window.__wipple_program} />
    </React.StrictMode>
);
