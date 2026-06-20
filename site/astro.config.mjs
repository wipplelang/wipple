// @ts-check
import { defineConfig } from "astro/config";
import tailwindcss from "@tailwindcss/vite";
import { resolve } from "node:path";
import { rehypeHeadingIds } from "@astrojs/markdown-remark";
import rehypeAutolinkHeadings from "rehype-autolink-headings";
import { readFileSync } from "node:fs";

export const wippleLanguage = JSON.parse(
    readFileSync("../ide/vscode/syntaxes/wipple.tmLanguage.json", "utf8"),
);

// https://astro.build/config
export default defineConfig({
    markdown: {
        rehypePlugins: [
            rehypeHeadingIds,
            [
                rehypeAutolinkHeadings,
                {
                    behavior: "wrap",
                    properties: { class: "no-underline" },
                },
            ],
        ],
        shikiConfig: {
            theme: "github-light",
            langs: [wippleLanguage],
            langAlias: {
                wipple: "Wipple",
                "wipple,playground": "Wipple",
            },
            wrap: null,
        },
    },
    vite: {
        plugins: [tailwindcss()],
        resolve: {
            alias: {
                "@": resolve("./src"),
            },
        },
    },
    integrations: [
        {
            name: "watch-guide",
            hooks: {
                "astro:server:setup": ({ server }) => {
                    server.watcher.add(resolve("../GUIDE.md"));
                },
            },
        },
    ],
});
