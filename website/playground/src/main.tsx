import { createTheme, ThemeProvider, useMediaQuery } from "@mui/material";
import React, { useMemo } from "react";
import ReactDOM from "react-dom/client";
import { ErrorBoundary as ErrorBoundaryComponent } from "react-error-boundary";
import App from "./App";
import { wipple } from "./languages";
import "./styles/globals.css";
import "./styles/language-wipple.css";
import ErrorIcon from "@mui/icons-material/Error";

wipple.register();

const ErrorBoundary = ErrorBoundaryComponent as any;

const Main = () => {
    const prefersDarkMode = useMediaQuery("(prefers-color-scheme: dark)");

    const theme = useMemo(
        () => createTheme({ palette: { mode: prefersDarkMode ? "dark" : "light" } }),
        [prefersDarkMode]
    );

    return (
        <React.StrictMode>
            <ThemeProvider theme={theme}>
                <ErrorBoundary fallbackRender={ErrorHandler}>
                    <App />
                </ErrorBoundary>
            </ThemeProvider>
        </React.StrictMode>
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
                        The Wipple Playground encountered an error. Please reload the page to
                        continue working. Your work will be saved.
                    </p>
                </div>

                <p className="text-sm text-gray-500 text-opacity-50">{error}</p>
            </div>
        </div>
    );
};

ReactDOM.createRoot(document.getElementById("root")!).render(<Main />);
