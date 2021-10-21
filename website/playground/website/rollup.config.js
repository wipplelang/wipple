import svelte from "rollup-plugin-svelte";
import commonjs from "@rollup/plugin-commonjs";
import resolve from "@rollup/plugin-node-resolve";
import livereload from "rollup-plugin-livereload";
import { terser } from "rollup-plugin-terser";
import sveltePreprocess from "svelte-preprocess";
import typescript from "@rollup/plugin-typescript";
import css from "rollup-plugin-css-only";
import rust from "@wasm-tool/rollup-plugin-rust";
import path from "path";
import glob from "fast-glob";

const production = !process.env.ROLLUP_WATCH;

function serve() {
    let server;

    function toExit() {
        if (server) server.kill(0);
    }

    return {
        writeBundle() {
            if (server) return;
            server = require("child_process").spawn(
                "npm",
                ["run", "start", "--", "--dev"],
                {
                    stdio: ["ignore", "inherit", "inherit"],
                    shell: true,
                }
            );

            process.on("SIGTERM", toExit);
            process.on("exit", toExit);
        },
    };
}

export default {
    input: "src/main.ts",
    output: {
        sourcemap: true,
        format: "iife",
        name: "app",
        file: "public/build/bundle.js",
    },
    plugins: [
        {
            name: "watch-src",
            buildStart: async function () {
                const src = path.resolve(__dirname, "../../../src");
                const files = await glob(src + "/**/*");

                files.forEach(this.addWatchFile);
            },
        },

        svelte({
            preprocess: sveltePreprocess({
                sourceMap: !production,
                postcss: {
                    plugins: [require("tailwindcss"), require("autoprefixer")],
                },
            }),
            compilerOptions: {
                dev: !production,
            },
        }),

        css({ output: "bundle.css" }),

        resolve({
            browser: true,
            dedupe: ["svelte"],
        }),

        commonjs(),

        typescript({
            sourceMap: !production,
            inlineSources: !production,
        }),

        rust({
            verbose: true,
            serverPath: "/build/",
        }),

        !production && serve(),

        !production && livereload("public"),

        production && terser(),
    ],
    watch: {
        clearScreen: false,
    },
};
