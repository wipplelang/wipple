import init from "./pkg/wipple_api";
export default init;

export * from "./pkg/wipple_api";

export const modulePath = new URL("./pkg/wipple_api_bg.wasm", import.meta.url);
