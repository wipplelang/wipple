module.exports = {
    content: ["./pages/**/*.{js,ts,jsx,tsx}", "./components/**/*.{js,ts,jsx,tsx}"],
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
                            color: theme("colors.gray.700"),
                            "border-radius": "0.25rem",
                            padding: "0.125rem",
                            fontWeight: "initial",
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
    variants: {},
    plugins: [require("@tailwindcss/typography")],
};
