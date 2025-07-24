/** @type {import("prettier").Config} */
export default {
    tabWidth: 4,
    printWidth: 100,
    proseWrap: "always",
    plugins: ["prettier-plugin-astro", "prettier-plugin-tailwindcss"],
    overrides: [
        {
            files: "*.astro",
            options: {
                parser: "astro",
            },
        },
    ],
};
