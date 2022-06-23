import { createTheme, ThemeProvider, useMediaQuery } from "@mui/material";
import { AppProps } from "next/app";
import { useMemo } from "react";
import { wipple } from "../languages";
import "../styles/globals.css";
import "../styles/language-wipple.css";

wipple.register();

const App = ({ Component, pageProps }: AppProps) => {
    const prefersDarkMode = useMediaQuery("(prefers-color-scheme: dark)");

    const theme = useMemo(
        () => createTheme({ palette: { mode: prefersDarkMode ? "dark" : "light" } }),
        [prefersDarkMode]
    );

    return (
        <ThemeProvider theme={theme}>
            <Component {...pageProps} />
        </ThemeProvider>
    );
};

export default App;
