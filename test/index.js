import test from "node:test";
import * as path from "node:path";
import { spawnSync } from "node:child_process";

const cmd = spawnSync(
    "target/debug/wipple",
    ["test", "--lib=library/src/foundation", "--explain", "--facts", "test/tests/*.wipple"],
    {
        stdio: ["ignore", "pipe", "inherit"],
    },
);

if (cmd.status !== 0) {
    throw new Error(`compiler exited with status code ${cmd.status}`);
}

const output = JSON.parse(cmd.stdout.toString("utf8"));

for (const entry of output) {
    test(entry.file, (t) => {
        t.assert.fileSnapshot(
            [entry.feedback, entry.output].join(""),
            path.join("test/tests", `${entry.file}.snap`),
            { serializers: [(s) => s] },
        );
    });
}
