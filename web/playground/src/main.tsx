import React from "react";
import ReactDOM from "react-dom/client";
import {
    Route,
    RouterProvider,
    createBrowserRouter,
    createRoutesFromElements,
} from "react-router-dom";
import { initializeApp } from "firebase/app";
import { HomePage, EditPage, RootPage } from "./pages";
import { NavbarProvider, AlertProvider } from "./components";
import { StoreProvider } from "./store";
import "react-material-symbols/rounded";
import "react-loading-skeleton/dist/skeleton.css";
import "react-piano/dist/styles.css";
import "react-resizable/css/styles.css";
import "./index.css";

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
        <Route path={import.meta.env.BASE_URL} element={<RootPage />}>
            <Route index element={<HomePage />} />
            <Route path="edit/:id/:page?" element={<EditPage />} />
        </Route>,
    ),
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
    </React.StrictMode>,
);
