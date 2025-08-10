import { MongoClient } from "mongodb";
import zod from "zod";
import { nanoid } from "nanoid";

const client = new MongoClient(process.env.MONGO_URI);
await client.connect();

const db = client.db(process.env.MONGO_DB_NAME);
const collection = db.collection("playgrounds");

// Expire documents after 1 week
await collection.createIndex("createdAt", { expireAfterSeconds: 7 * 86400 });

export default async (req) => {
    if (req.method === "OPTIONS") {
        return cors(new Response());
    }

    const data = zod
        .discriminatedUnion("type", [
            zod.object({
                type: zod.literal("get"),
                id: zod.string(),
            }),
            zod.object({
                type: zod.literal("share"),
                runtime: zod.string(),
                code: zod.string(),
            }),
        ])
        .parse(await req.json());

    let res;
    switch (data.type) {
        case "get": {
            const playground = await collection.findOne({ _id: data.id });
            if (!playground) {
                throw new Error("playground not found");
            }

            res = new Response(JSON.stringify({ playground }));
            break;
        }
        case "share": {
            const id = nanoid(6);

            await db.collection("playgrounds").insertOne({
                _id: id,
                createdAt: new Date(),
                runtime: data.runtime,
                code: data.code,
            });

            res = new Response(JSON.stringify({ id }));
            break;
        }
        default:
            throw new Error("invalid request");
    }

    return cors(res);
};

const cors = (res) => {
    res.headers.set("Access-Control-Allow-Origin", "*");
    res.headers.set("Access-Control-Allow-Methods", "POST, OPTIONS");
    res.headers.set("Access-Control-Allow-Headers", "Content-Type");
    return res;
};
