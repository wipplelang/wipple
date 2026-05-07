const path = require("node:path");
const fs = require("node:fs");
const process = require("node:process");
const { spawnSync } = require("node:child_process");

process.chdir(__dirname);

fs.rmSync("dist", { recursive: true, force: true });
fs.mkdirSync("dist", { recursive: true });

const buildCmd = spawnSync("swift", ["build", "--package-path=compiler", "--product", "wipple"], {
    cwd: path.resolve(__dirname, ".."),
    stdio: "inherit",
});

if (buildCmd.status !== 0) {
    throw new Error(`build exited with status code ${buildCmd.status}`);
}

for (let dir of fs.readdirSync("src")) {
    if (!fs.statSync(path.join("src", dir)).isDirectory()) {
        fs.copyFileSync(path.join("src", dir), path.join("dist", dir));
        continue;
    }

    const metadata = JSON.parse(fs.readFileSync(path.join("src", dir, "_metadata.json"), "utf8"));

    const files = fs
        .readdirSync(path.join("src", dir))
        .filter((file) => file.endsWith(".wipple"))
        .map((file) => ({
            path: path.join(dir, file),
            code: fs.readFileSync(path.join("src", dir, file), "utf8"),
        }));

    const cmd = spawnSync(
        "./compiler/.build/debug/wipple",
        [
            "doc",
            ...files.map((file) => path.join(__dirname, "src", file.path)),
            ...(metadata.library ? ["--lib", path.join(__dirname, "src", metadata.library)] : []),
        ],
        {
            cwd: path.resolve(__dirname, ".."),
            stdio: ["ignore", "pipe", "inherit"],
        },
    );

    if (cmd.status !== 0) {
        throw new Error(`compiler exited with status code ${cmd.status}`);
    }

    const docs = JSON.parse(cmd.stdout.toString("utf8"));

    fs.writeFileSync(path.join("dist", `${dir}.json`), JSON.stringify({ metadata, files, docs }));
}
