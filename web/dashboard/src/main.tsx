import ReactDOM from "react-dom/client";
import { initializeApp } from "firebase/app";
import {
    browserLocalPersistence,
    browserSessionPersistence,
    indexedDBLocalPersistence,
    initializeAuth,
} from "firebase/auth";
import {
    initializeFirestore,
    persistentLocalCache,
    persistentMultipleTabManager,
} from "firebase/firestore";
import * as Sentry from "@sentry/react";
import ReactGA from "react-ga4";
import { NavbarProvider, AlertProvider } from "wipple-playground";
import { App } from "./app";
import "wipple-playground/dist/style.css";
import "./index.css";
import { StoreProvider } from "./store";

if (import.meta.env.DEV) {
    localStorage.debug = "*";
}

if (import.meta.env.PROD) {
    Sentry.init({
        dsn: import.meta.env.VITE_SENTRY_DSN,
        integrations: [Sentry.browserTracingIntegration(), Sentry.replayIntegration()],
        tracesSampleRate: 1.0,
        tracePropagationTargets: ["localhost"],
        replaysSessionSampleRate: 0.1,
        replaysOnErrorSampleRate: 1.0,
    });
}

ReactGA.initialize(import.meta.env.VITE_GA_MEASUREMENT_ID);

const app = initializeApp({
    apiKey: import.meta.env.VITE_FIREBASE_API_KEY,
    authDomain: import.meta.env.VITE_FIREBASE_AUTH_DOMAIN,
    projectId: import.meta.env.VITE_FIREBASE_PROJECT_ID,
    storageBucket: import.meta.env.VITE_FIREBASE_STORAGE_BUCKET,
    messagingSenderId: import.meta.env.VITE_FIREBASE_MESSAGING_SENDER_ID,
    appId: import.meta.env.VITE_FIREBASE_APP_ID,
    measurementId: import.meta.env.VITE_FIREBASE_MEASUREMENT_ID,
});

initializeAuth(app, {
    persistence: [indexedDBLocalPersistence, browserLocalPersistence, browserSessionPersistence],
});

initializeFirestore(app, {
    localCache: persistentLocalCache({ tabManager: persistentMultipleTabManager() }),
});

ReactDOM.createRoot(document.getElementById("root")!).render(
    <StoreProvider>
        <AlertProvider>
            <NavbarProvider>
                <App />
            </NavbarProvider>
        </AlertProvider>
    </StoreProvider>,
);
