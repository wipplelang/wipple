import { defineConfig } from "vite";
import react from "@vitejs/plugin-react-swc";
import { lezer } from "@lezer/generator/rollup";
import svgr from "vite-plugin-svgr";
import topLevelAwait from "vite-plugin-top-level-await";
import { VitePWA as pwa, Options as PWAOptions } from "vite-plugin-pwa";

const pwaOptions: Partial<PWAOptions> = {
    injectRegister: "inline",
    registerType: "autoUpdate",
    manifest: false,
    workbox: {
        globPatterns: ["**/*.{js,css,html,ico,jpg,png,svg,woff2,json,wipplebundle}"],
        maximumFileSizeToCacheInBytes: 20 * 1024 * 1024, // 20 MB
    },
    devOptions: {
        enabled: true,
    },
};

// https://vitejs.dev/config/
export default defineConfig({
    base: "/playground",
    plugins: [react(), lezer(), svgr(), topLevelAwait(), pwa(pwaOptions)],
    server: {
        port: 8080,
    },
});
