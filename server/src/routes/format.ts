import * as z from "zod";
import compiler from "compiler";
import { handler } from "../handler";

const FormatRequest = z.object({
    code: z.string(),
});

export default handler(FormatRequest, async (body) => {
    const formatted = compiler.format(body.code);

    return {
        statusCode: 200,
        body: { code: formatted != null && formatted !== body.code ? formatted : null },
    };
});
