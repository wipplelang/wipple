import { plugin, type BunPlugin } from "bun";

const wipple: BunPlugin = {
    name: "Wipple test loader",
    setup: async (build) => {
        build.onLoad({ filter: /\.wipple$/ }, async (args) => {
            const text = await Bun.file(args.path).text();

            return {
                contents: `
import path from "path";
import { test, expect } from "bun:test";

const { runTest } = await import(path.resolve(process.cwd(), "./src/run-test.ts"));

test(path.basename(${JSON.stringify(args.path)}), async () => {
    await runTest(
        ${JSON.stringify(args.path)},
        ${JSON.stringify(text)},
        (left, right) => expect(left).toBe(right),
        (value) => expect(value).toMatchSnapshot(),
    );
});
`,
                loader: "js",
            };
        });
    },
};

plugin(wipple);
