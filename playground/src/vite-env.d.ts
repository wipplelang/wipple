/// <reference types="vite/client" />

declare module "*.md" {
    export const attributes: Record<string, unknown>;
    export const markdown: string;
}
