/// <reference types="vite/client" />
/// <reference types="vite-plugin-svgr/client" />

declare module "*.md" {
    export const attributes: Record<string, unknown>;
    export const markdown: string;
}
