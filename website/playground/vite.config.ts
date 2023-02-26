import { defineConfig } from "vite";
import react from "@vitejs/plugin-react-swc";
import wasm from "vite-plugin-wasm";
import topLevelAwait from "vite-plugin-top-level-await";
import { VitePWA } from "vite-plugin-pwa";

export default defineConfig({
    plugins: [
        react(),
        wasm(),
        topLevelAwait(),
        VitePWA({
            registerType: "autoUpdate",
            devOptions: {
                enabled: true,
            },
            includeAssets: ["images/**/*"],
            workbox: {
                globPatterns: ["**/*.{js,css,html,wasm,json}"],
                maximumFileSizeToCacheInBytes: 10 * 1024 ** 2, // 10 MB
            },
            manifest: {
                name: "Wipple Playground",
                short_name: "Wipple",
                description: "Learn and experiment with the Wipple programming language",
                icons: [
                    {
                        src: "./images/logo-192.png",
                        sizes: "192x192",
                        type: "image/png",
                    },
                    {
                        src: "./images/logo-512.png",
                        sizes: "512x512",
                        type: "image/png",
                    },
                ],
            },
        }),
    ],
    worker: {
        format: "es",
        plugins: [wasm(), topLevelAwait()],
    },
    server: {
        port: 3000,
    },
    base: "/playground",
});
