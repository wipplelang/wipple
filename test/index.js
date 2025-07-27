import test from "node:test";
import fs from "node:fs";
import path from "node:path";
import vm from "node:vm";
import runtime from "../runtime/runtime.js";

const testsPath = path.join(import.meta.dirname, "tests");

const testsServerUrl = process.env.TESTS_SERVER_URL;
if (!testsServerUrl) {
    throw new Error("TESTS_SERVER_URL not set");
}

for (const testFile of fs.readdirSync(testsPath)) {
    test(testFile, async (t) => {
        const code = fs.readFileSync(path.join(testsPath, testFile), "utf8");

        const shouldCompile = code.startsWith("-- [should compile]");
        const shouldError = code.startsWith("-- [should error]");

        if (shouldCompile === shouldError) {
            throw new Error("expected either a `[should compile]` or a `[should error]` comment");
        }

        const response = await fetch(testsServerUrl, {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ compile: { library: "foundation", code } }),
        }).then((response) => response.json());

        if (response.success) {
            const module = new vm.SourceTextModule(response.executable, {
                identifier: `compiled ${testFile}`,
            });

            await module.link(() => {});
            await module.evaluate();

            const entrypoint = module.namespace.default;

            let output = "";
            const env = {
                display: (message) => {
                    output += message + "\n";
                },
                debug: (value) => console.error("--- debug:", value),
            };

            await entrypoint(runtime(env));

            t.assert.snapshot({ success: true, output });
        } else {
            t.assert.snapshot({ success: false, diagnostics: response.diagnostics });
        }

        t.assert.equal(response.success, shouldCompile);
    });
}
