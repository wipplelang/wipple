import { Trouter, type Methods } from "trouter";
import { createServer } from "node:http";
import routes from "./routes";
import type { Handler } from "./handler";

export const router = new Trouter<Handler>();
for (const [path, handler] of Object.entries(routes)) {
    router.post(path, handler);
}

const handle = async (method: string, path: string, body: string | undefined) => {
    const {
        handlers: [handler],
    } = router.find(method as Methods, path);

    if (handler == null) {
        return { statusCode: 404, body: "not found" };
    }

    return handler(body)!;
};

export default async (req: Request) => {
    const path = new URL(req.url).pathname.replace("/.netlify/functions/api", "");
    console.log(req.method, path);

    const body = await req.text();
    const response = await handle(req.method, path, body);
    return new Response(response.body, { status: response.statusCode });
};

export const runServer = () => {
    const server = createServer(async (req, res) => {
        const { method, url } = req;

        res.setHeader("Access-Control-Allow-Origin", "*");
        res.setHeader("Access-Control-Allow-Methods", "POST, OPTIONS");
        res.setHeader("Access-Control-Allow-Headers", "Content-Type");

        if (method === "OPTIONS") {
            res.statusCode = 204;
            res.end();
            return;
        }

        if (method !== "POST") {
            res.statusCode = 405;
            res.end();
            return;
        }

        req.setEncoding("utf8");

        let body = "";
        for await (const chunk of req) {
            body += chunk;
        }

        const response = await handle(method!, url!, body);

        res.statusCode = response.statusCode;
        res.setHeader("Content-Type", "application/json");
        res.end(response.body);
    });

    if (!process.env.PORT) {
        throw new Error("missing PORT");
    }

    server.listen(process.env.PORT, () => {
        console.log("listening on port", process.env.PORT);
    });
};
