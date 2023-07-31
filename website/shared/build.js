export const rustOptions = (env) => ({
    debug: false,
    cargoArgs: env.CI ? [] : ["--features", "debug_playground"],
    wasmOptArgs: env.CI ? ["-O1"] : [],
    verbose: true,
    inlineWasm: true,
    experimental: {
        synchronous: true,
    },
});
