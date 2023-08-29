import { createTheme, ThemeProvider, useMediaQuery } from "@mui/material";
import React, { useMemo } from "react";
import ReactDOM from "react-dom/client";
import * as Sentry from "@sentry/react";
import App from "./App";
import { wipple } from "./languages";
import "./styles/globals.css";
import "./styles/language-wipple.css";
import "katex/dist/katex.min.css";
import ErrorIcon from "@mui/icons-material/Error";
import "react-piano/dist/styles.css";

wipple.register();

const Main = () => {
    const prefersDarkMode = useMediaQuery("(prefers-color-scheme: dark)");

    const theme = useMemo(
        () => createTheme({ palette: { mode: prefersDarkMode ? "dark" : "light" } }),
        [prefersDarkMode]
    );

    return (
        <ThemeProvider theme={theme}>
            <Sentry.ErrorBoundary fallback={ErrorHandler}>
                <App />
            </Sentry.ErrorBoundary>
        </ThemeProvider>
    );
};

const ErrorHandler = (props: { error: any }) => {
    const error =
        typeof props.error === "string"
            ? props.error
            : props.error.toString?.() ?? JSON.stringify(props.error);

    return (
        <div className="w-screen h-screen bg-red-50 dark:bg-red-900 dark:bg-opacity-20">
            <div className="flex w-full max-w-screen-md mx-auto h-full flex-col items-center justify-between text-center p-4">
                <div className="flex flex-col items-center justify-center flex-1 gap-4 text-red-500">
                    <ErrorIcon sx={{ fontSize: 60 }} />

                    <h1 className="text-xl">Something went wrong</h1>

                    <p className="text-gray-500">
                        Wipple encountered an error. Please reload the page to continue working.
                        Your work will be saved.
                    </p>

                    {/Chrome/.test(navigator.userAgent) ? null : (
                        <p className="text-gray-500">
                            If you continue having issues, please try updating your browser to the
                            latest version or using Google Chrome.
                        </p>
                    )}
                </div>

                <div className="text-sm text-gray-500 text-opacity-50">
                    <p>{error}</p>
                    <p>User agent: {navigator.userAgent}</p>
                </div>
            </div>
        </div>
    );
};

if (import.meta.env.PROD) {
    Sentry.init({
        dsn: import.meta.env.VITE_SENTRY_DSN,
        autoSessionTracking: false,
    });
}

ReactDOM.createRoot(document.getElementById("root")!).render(<Main />);
