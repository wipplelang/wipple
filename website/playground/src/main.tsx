import { createTheme, ThemeProvider, useMediaQuery } from "@mui/material";
import React, { useMemo } from "react";
import ReactDOM from "react-dom/client";
import App from "./App";
import { wipple } from "./languages";
import "./styles/globals.css";
import "./styles/language-wipple.css";

wipple.register();

const Main = () => {
    const prefersDarkMode = useMediaQuery("(prefers-color-scheme: dark)");

    const theme = useMemo(
        () => createTheme({ palette: { mode: prefersDarkMode ? "dark" : "light" } }),
        [prefersDarkMode]
    );

    return (
        <React.StrictMode>
            <ThemeProvider theme={theme}>
                <App />
            </ThemeProvider>
        </React.StrictMode>
    );
};

ReactDOM.createRoot(document.getElementById("root")!).render(<Main />);
