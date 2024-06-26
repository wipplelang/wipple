import ReactDOM from "react-dom/client";
import {
    Route,
    RouterProvider,
    createBrowserRouter,
    createRoutesFromElements,
} from "react-router-dom";
import { initializeApp } from "firebase/app";
import {
    initializeFirestore,
    persistentLocalCache,
    persistentMultipleTabManager,
} from "firebase/firestore";
import * as Sentry from "@sentry/react";
import ReactGA from "react-ga4";
import { HomePage, EditPage, RootPage, LessonPage } from "./pages";
import { NavbarProvider, AlertProvider } from "./components";
import { StoreProvider } from "./store";
import "react-material-symbols/rounded";
import "react-loading-skeleton/dist/skeleton.css";
import "react-piano/dist/styles.css";
import "react-resizable/css/styles.css";
import "katex/dist/katex.min.css";
import "./index.css";

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

initializeFirestore(app, {
    localCache: persistentLocalCache({ tabManager: persistentMultipleTabManager() }),
});

const router = createBrowserRouter(
    createRoutesFromElements(
        <Route path={import.meta.env.BASE_URL} element={<RootPage />}>
            <Route index element={<HomePage />} />
            <Route path="edit/:id/:page?" element={<EditPage />} />
            <Route path="lesson/:id/:page?" element={<LessonPage />} />
        </Route>,
    ),
);

ReactDOM.createRoot(document.getElementById("root")!).render(
    <StoreProvider>
        <AlertProvider>
            <NavbarProvider>
                <RouterProvider router={router} />
            </NavbarProvider>
        </AlertProvider>
    </StoreProvider>,
);
