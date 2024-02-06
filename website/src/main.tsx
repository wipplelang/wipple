import React from "react";
import ReactDOM from "react-dom/client";
import {
    Route,
    RouterProvider,
    createBrowserRouter,
    createRoutesFromElements,
} from "react-router-dom";
import { initializeApp } from "firebase/app";
import { Home, Playground, Root } from "./pages";
import { NavbarProvider, AlertProvider } from "./components";
import "react-material-symbols/outlined";
import "react-loading-skeleton/dist/skeleton.css";
import "./index.css";
import { StoreProvider } from "./store";

initializeApp({
    apiKey: import.meta.env.VITE_FIREBASE_API_KEY,
    authDomain: import.meta.env.VITE_FIREBASE_AUTH_DOMAIN,
    projectId: import.meta.env.VITE_FIREBASE_PROJECT_ID,
    storageBucket: import.meta.env.VITE_FIREBASE_STORAGE_BUCKET,
    messagingSenderId: import.meta.env.VITE_FIREBASE_MESSAGING_SENDER_ID,
    appId: import.meta.env.VITE_FIREBASE_APP_ID,
    measurementId: import.meta.env.VITE_FIREBASE_MEASUREMENT_ID,
});

const router = createBrowserRouter(
    createRoutesFromElements(
        <Route path="/" element={<Root />}>
            <Route index element={<Home />} />
            <Route path="playground/:id" element={<Playground />} />
        </Route>
    )
);

ReactDOM.createRoot(document.getElementById("root")!).render(
    <React.StrictMode>
        <StoreProvider>
            <AlertProvider>
                <NavbarProvider>
                    <RouterProvider router={router} />
                </NavbarProvider>
            </AlertProvider>
        </StoreProvider>
    </React.StrictMode>
);
