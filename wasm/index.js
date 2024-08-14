import init, {
    initialize,
    compile,
    format,
    highlights,
    help,
    getIntelligentFix,
    run,
    stop,
    cleanup,
} from "./pkg/wipple_wasm.js";

await init();

export { initialize, compile, format, highlights, help, getIntelligentFix, run, stop, cleanup };
