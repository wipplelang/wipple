export default {
    plugins: {
        "@tailwindcss/postcss": {},
        autoprefixer: {},
        "@csstools/postcss-oklab-function": {
            subFeatures: { displayP3: false },
        },
        "@csstools/postcss-color-mix-function": {},
    },
};
