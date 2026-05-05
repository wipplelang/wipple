import * as z from "zod";

interface HandlerResponse<T> {
    statusCode: number;
    body: T;
}

export type Handler = (body: string | undefined) => Promise<HandlerResponse<string>>;

export const handler =
    <T extends z.ZodType>(
        schema: T,
        f: (body: z.infer<T>) => Promise<HandlerResponse<Record<string, unknown>>>,
    ): Handler =>
    async (body) => {
        const result = schema.safeParse(body && JSON.parse(body));
        if (!result.success) {
            return {
                statusCode: 400,
                body: JSON.stringify(z.treeifyError(result.error)),
            };
        }

        try {
            const { statusCode, body } = await f(result.data);
            return { statusCode, body: JSON.stringify(body) };
        } catch (error: any) {
            console.error(error);

            return { statusCode: 500, body: JSON.stringify({ error: error.toString() }) };
        }
    };
