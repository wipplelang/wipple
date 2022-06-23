/** @type {import('tailwindcss').Config} */
module.exports = {
    content: ["./pages/**/*.{js,ts,jsx,tsx}", "./components/**/*.{js,ts,jsx,tsx}"],
    darkMode: "media",
    theme: {
        extend: {
            typography: (theme) => ({
                DEFAULT: {
                    css: {
                        pre: {
                            "background-color": theme("colors.gray.50"),
                        },
                        "pre code::before": {
                            "padding-left": "unset",
                        },
                        "pre code::after": {
                            "padding-right": "unset",
                        },
                        code: {
                            backgroundColor: "var(--code-bg)",
                            color: "var(--code-fg)",
                            "border-radius": "0.25rem",
                            padding: "0.125rem",
                            fontWeight: "initial",
                        },
                        "pre code": {
                            backgroundColor: "var(--code-bg)",
                            color: "var(--code-fg)",
                        },
                        "code::before": {
                            content: '""',
                            "padding-left": "0.25rem",
                        },
                        "code::after": {
                            content: '""',
                            "padding-right": "0.25rem",
                        },
                    },
                },
            }),
        },
    },
    plugins: [require("@tailwindcss/typography")],
};
