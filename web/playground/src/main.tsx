import ReactDOM from "react-dom/client";
import {
    Route,
    RouterProvider,
    createBrowserRouter,
    createRoutesFromElements,
    useRouteError,
} from "react-router-dom";
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
import { HomePage, EditPage, RootPage } from "./pages";
import { AlertProvider } from "./components";
import { StoreProvider } from "./store";
import "@fontsource-variable/inter";
import "@fontsource-variable/inter/wght-italic.css";
import "@fontsource-variable/jetbrains-mono";
import "@fontsource-variable/jetbrains-mono/wght-italic.css";
import "react-material-symbols/rounded";
import "react-loading-skeleton/dist/skeleton.css";
import "react-piano/dist/styles.css";
import "react-resizable/css/styles.css";
import "katex/dist/katex.min.css";
import "./index.css";
import { ErrorScreen } from "./components/error-screen";
import { useEffect } from "react";

if (import.meta.env.PROD) {
    Sentry.init({
        dsn: import.meta.env.VITE_SENTRY_DSN,
        integrations: [Sentry.browserTracingIntegration()],
        tracesSampleRate: 1.0,
        tracePropagationTargets: ["localhost"],
    });

    ReactGA.initialize(import.meta.env.VITE_GA_MEASUREMENT_ID);
}

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

const ErrorBoundary = () => {
    const error = useRouteError() as Error;

    useEffect(() => {
        Sentry.captureException(error);
    }, [error]);

    return <ErrorScreen error={error} />;
};

const router = createBrowserRouter(
    createRoutesFromElements(
        <Route
            path={import.meta.env.BASE_URL}
            element={<RootPage />}
            errorElement={<ErrorBoundary />}
        >
            <Route index element={<HomePage />} />
            <Route path="edit/:id" element={<EditPage />} />
        </Route>,
    ),
);

ReactDOM.createRoot(document.getElementById("root")!).render(
    <StoreProvider>
        <AlertProvider>
            <RouterProvider router={router} />
        </AlertProvider>
    </StoreProvider>,
);
