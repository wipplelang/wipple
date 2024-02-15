import { defineConfig } from "vite";
import react from "@vitejs/plugin-react-swc";
import { lezer } from "@lezer/generator/rollup";

// https://vitejs.dev/config/
export default defineConfig({
    plugins: [react(), lezer()],
    server: {
        port: 8080,
    },
});
