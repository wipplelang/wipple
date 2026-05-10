import { WASI } from "node:wasi";
import { readFileSync } from "node:fs";
import { instantiate } from "./.build-wasm/plugins/PackageToJS/outputs/Package/instantiate.js";

const wasi = new WASI({ version: "preview1" });

const moduleUrl = new URL(
    "./.build-wasm/plugins/PackageToJS/outputs/Package/API.wasm",
    import.meta.url,
);

const options = {
    module: readFileSync(moduleUrl),
    getImports: () => {
        throw new Error("no imports");
    },
    wasi,
};

const { exports } = await instantiate(options);

export default exports;
