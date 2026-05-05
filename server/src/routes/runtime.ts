import * as z from "zod";
import { handler } from "../handler";
import { runtime } from "../library";

const RuntimeRequest = z.object({});

export default handler(RuntimeRequest, async (_body) => {
    return { statusCode: 200, body: { runtime } };
});
