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
                            backgroundColor: theme("colors.gray.50"),
                            color: theme("colors.gray.900"),
                            "border-radius": "0.25rem",
                            padding: "0.125rem",
                            fontWeight: "initial",
                        },
                        "pre code": {
                            backgroundColor: theme("colors.gray.50"),
                            color: theme("colors.gray.900"),
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
                invert: {
                    css: {
                        pre: {
                            "background-color": theme("colors.gray.800"),
                        },
                        code: {
                            backgroundColor: theme("colors.gray.800"),
                            color: theme("colors.gray.50"),
                        },
                        "pre code": {
                            backgroundColor: theme("colors.gray.800"),
                            color: theme("colors.gray.50"),
                        },
                    },
                },
            }),
        },
    },
    plugins: [require("@tailwindcss/typography")],
};
