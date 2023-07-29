export const rustOptions = (env) => ({
    cargoArgs: env.CI ? [] : ["--features", "debug_playground"],
    wasmOptArgs: ["-O1"],
    verbose: true,
    inlineWasm: true,
    experimental: {
        synchronous: true,
    },
});
