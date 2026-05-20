const path = require("node:path");
const fs = require("node:fs");
const process = require("node:process");
const { spawnSync } = require("node:child_process");

process.chdir(__dirname);

fs.rmSync("dist", { recursive: true, force: true });
fs.mkdirSync("dist", { recursive: true });

let compiledFoundation = false;
for (let dir of ["foundation", ...fs.readdirSync("src")]) {
    if (dir === "foundation" && compiledFoundation) {
        continue;
    }

    if (!fs.statSync(path.join("src", dir)).isDirectory()) {
        fs.copyFileSync(path.join("src", dir), path.join("dist", dir));
        continue;
    }

    const getMetadata = (lib) =>
        JSON.parse(fs.readFileSync(path.join("src", lib, "_metadata.json"), "utf8"));

    const metadata = getMetadata(dir);

    const files = fs
        .readdirSync(path.join("src", dir))
        .filter((file) => file.endsWith(".wipple"))
        .map((file) => ({
            path: path.join(dir, file),
            code: fs.readFileSync(path.join("src", dir, file), "utf8"),
        }));

    const compileCmd = spawnSync(
        "cargo",
        [
            "run",
            "--",
            "compile",
            ...(compiledFoundation
                ? [`--lib=${path.join(__dirname, "dist", "foundation.bin")}`]
                : []),
            ...files.map((file) =>
                path.relative(
                    path.resolve(__dirname, ".."),
                    path.join(__dirname, "src", file.path),
                ),
            ),
            `--lib-artifact=${path.join(__dirname, "dist", `${dir}.bin`)}`,
        ],
        {
            cwd: path.resolve(__dirname, ".."),
            stdio: "inherit",
        },
    );

    if (compileCmd.status !== 0) {
        throw new Error(`compiler exited with status code ${compileCmd.status}`);
    }

    const docCmd = spawnSync(
        "cargo",
        ["run", "--", "doc", `--lib=${path.join(__dirname, "dist", `${dir}.bin`)}`],
        {
            cwd: path.resolve(__dirname, ".."),
            stdio: ["ignore", "pipe", "inherit"],
        },
    );

    if (docCmd.status !== 0) {
        throw new Error(`compiler exited with status code ${docCmd.status}`);
    }

    const docs = JSON.parse(docCmd.stdout.toString("utf8"));

    fs.writeFileSync(path.join("dist", `${dir}.json`), JSON.stringify({ metadata, docs }));

    compiledFoundation = true;
}
