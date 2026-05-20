import * as z from "zod";
import * as compiler from "compiler";
import { InputMetadata, loadLibrary } from "../library";
import { handler } from "../handler";

const CompileRequest = z.intersection(
    InputMetadata,
    z.object({
        code: z.string(),
        groups: z.boolean().optional(),
        graph: z.boolean().optional(),
    }),
);

export default handler(CompileRequest, async (body) => {
    if (body.library != null) {
        loadLibrary(body.library);
    }

    using result = compiler.compile([new compiler.File("input", body.code)], body.library);
    if (result == null) {
        throw new Error("compilation failed");
    }

    const graph = body.graph ? result.graph() : undefined;

    const diagnostics = result.diagnostics();
    if (diagnostics != null) {
        return { statusCode: 200, body: { graph, diagnostics } };
    }

    const groups = body.groups ? result.groups() : undefined;

    const executableBase64 = result.executable();
    if (executableBase64 == null) {
        throw new Error("missing executable");
    }

    return { statusCode: 200, body: { groups, graph, executable: executableBase64 } };
});
