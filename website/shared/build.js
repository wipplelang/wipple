export const rustOptions = (env) => ({
    debug: false,
    wasmOptArgs: env.CI ? ["-O1"] : [],
    verbose: true,
    inlineWasm: true,
    experimental: {
        synchronous: true,
    },
});
