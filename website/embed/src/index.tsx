import { createTheme, ThemeProvider, useMediaQuery } from "@mui/material";
import { useEffect, useMemo } from "react";
import ReactDOM from "react-dom/client";
import { Globals as SpringGlobals } from "react-spring";
import { PlaygroundRunner } from "shared";
import "./styles/globals.css";
import "katex/dist/katex.min.css";

const App = (props: { code: string }) => {
    const prefersDarkMode = useMediaQuery("(prefers-color-scheme: dark)");

    const theme = useMemo(
        () => createTheme({ palette: { mode: prefersDarkMode ? "dark" : "light" } }),
        [prefersDarkMode]
    );

    const prefersReducedMotion = useMediaQuery("(prefers-reduced-motion)");
    useEffect(() => {
        SpringGlobals.assign({
            skipAnimation: prefersReducedMotion,
        });
    }, [prefersReducedMotion]);

    return (
        <ThemeProvider theme={theme}>
            <div id="code-editor-container">
                <PlaygroundRunner
                    id="embed"
                    code={props.code}
                    beginner
                    lint={false}
                    autoRun
                    containsTemplates={() => false}
                    footer={Footer}
                />
            </div>
        </ThemeProvider>
    );
};

const Footer = () => (
    <div className="px-4 pb-4 text-xs">
        <a
            target="_blank"
            href="https://wipple.dev"
            className="flex items-center gap-1.5 text-gray-500 dark:text-gray-400"
        >
            <img src="https://wipple.dev/images/logo.svg" className="w-4 h-4" />
            Powered by Wipple
        </a>
    </div>
);

(() => {
    const codeElements = document.querySelectorAll<HTMLElement>("wipple");
    if (codeElements.length === 0) {
        console.error(
            "Error: Could not find Wipple code to run. Please make sure your page contains one or more <wipple> elements."
        );

        return;
    }

    for (const codeElement of codeElements) {
        const scriptElement = codeElement.querySelector<HTMLScriptElement>(
            'script[type="application/wipple"]'
        );

        if (!scriptElement) {
            console.error(
                'Error: Could not find <script type="application/wipple"> element inside',
                codeElement
            );

            continue;
        }

        codeElement.style.display = "block";

        const outputElement = document.createElement("div");
        outputElement.id = "output";
        codeElement.appendChild(outputElement);

        ReactDOM.createRoot(outputElement).render(<App code={scriptElement.text} />);
    }
})();
