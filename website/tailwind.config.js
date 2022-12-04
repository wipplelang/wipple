/** @type {import('tailwindcss').Config} */
module.exports = {
    content: ["./home/**/*.{html,md}"],
    plugins: [require("@tailwindcss/typography")],
    darkMode: "media",
    theme: {
        extend: {
            typography: {
                DEFAULT: {
                    css: {
                        pre: false,
                        code: false,
                        "pre code": false,
                        "code::before": false,
                        "code::after": false,
                        "blockquote p:first-of-type::before": false,
                        "blockquote p:last-of-type::after": false,
                    },
                },
            },
        },
    },
    variants: {
        typography: ["responsive", "dark"],
        extend: {
            typography: ["dark"],
        },
    },
};
