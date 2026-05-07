import * as z from "zod";
import compiler, { type CompileResult } from "compiler";
import * as wat from "wat-parser";
import { InputMetadata, loadLibrary } from "../library";
import { handler } from "../handler";

const CompileRequest = z.intersection(
    InputMetadata,
    z.object({
        code: z.string(),
        graph: z.boolean().optional(),
    }),
);

export default handler(CompileRequest, async (body) => {
    let result: CompileResult | null = null;
    try {
        if (body.library != null) {
            loadLibrary(body.library);
        }

        result = compiler.compile([{ path: "input", code: body.code }], body.library);
        if (result == null) {
            throw new Error("compilation failed");
        }

        const graph = body.graph ? result.graph() : undefined;

        const diagnostics = result.diagnostics();
        if (diagnostics != null) {
            return { statusCode: 200, body: { graph, diagnostics } };
        }

        const executable = result.executable();
        if (executable == null) {
            throw new Error("missing executable");
        }

        const executableBase64 = wat.parse(executable);
        if (executableBase64 == null) {
            throw new Error("failed to parse WebAssembly executable");
        }

        return { statusCode: 200, body: { graph, executable: executableBase64 } };
    } finally {
        result?.release();
    }
});
