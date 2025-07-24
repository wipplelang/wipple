// @ts-check
import { defineConfig } from "astro/config";
import tailwindcss from "@tailwindcss/vite";
import { resolve } from "node:path";

// https://astro.build/config
export default defineConfig({
    vite: {
        plugins: [tailwindcss()],
        resolve: {
            alias: {
                "@": resolve("./src"),
            },
        },
    },
});
