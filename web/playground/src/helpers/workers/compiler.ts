import * as compiler from "wipple-compiler";

onmessage = async (event) => {
    const { type } = event.data;
    switch (type) {
        case "compile": {
            const { code, dependencies } = event.data;

            const sources = [
                {
                    path: "playground",
                    visiblePath: "playground",
                    code,
                },
            ];

            const compileResult = compiler.compile(sources, dependencies?.interface ?? null);

            const executable = compiler.link([
                ...(dependencies?.libraries ?? []),
                compileResult.library,
            ]);

            postMessage({ type: "completion", compileResult, executable });

            break;
        }
        case "format": {
            const code = event.data.code;
            const formatted = compiler.format(code);
            postMessage({ type: "completion", code: formatted });
            break;
        }
        default:
            throw new Error(`unsupported message: ${type}`);
    }
};
