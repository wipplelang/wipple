import * as path from "node:path";
import * as fs from "node:fs";
import * as process from "node:process";
import { spawn } from "node:child_process";
import { once } from "node:events";

const { dirname } = import.meta;

process.chdir(dirname);

fs.rmSync("dist", { recursive: true, force: true });
fs.mkdirSync("dist", { recursive: true });

const compile = async (dir, dependencies) => {
    if (!fs.statSync(path.join("src", dir)).isDirectory()) {
        fs.copyFileSync(path.join("src", dir), path.join("dist", dir));
        return;
    }

    console.log("Building", dir);

    const getMetadata = (lib) =>
        JSON.parse(fs.readFileSync(path.join("src", lib, "_metadata.json"), "utf8"));

    const metadata = getMetadata(dir);

    const compileCmd = spawn(
        "target/debug/wipple",
        [
            "compile",
            ...dependencies.map(
                (dependency) => `--lib=${path.join(dirname, "dist", `${dependency}.bin`)}`,
            ),
            path.join(dirname, "src", dir) + "/*.wipple",
            `--lib-artifact=${path.join(dirname, "dist", `${dir}.bin`)}`,
            "--silent",
        ],
        {
            cwd: path.resolve(dirname, ".."),
            stdio: "inherit",
        },
    );

    const [compileStatus] = await once(compileCmd, "close");

    if (compileStatus !== 0) {
        throw new Error(`compiler exited with status code ${compileStatus}`);
    }

    const docCmd = spawn(
        "target/debug/wipple",
        ["doc", `--lib=${path.join(dirname, "dist", `${dir}.bin`)}`, "--silent"],
        {
            cwd: path.resolve(dirname, ".."),
            stdio: ["ignore", "pipe", "inherit"],
        },
    );

    let docs = "";
    docCmd.stdout.on("data", (chunk) => {
        docs += chunk.toString();
    });

    const [docStatus] = await once(docCmd, "close");

    if (docStatus !== 0) {
        throw new Error(`compiler exited with status code ${docStatus}`);
    }

    fs.writeFileSync(
        path.join("dist", `${dir}.json`),
        JSON.stringify({ metadata, docs: JSON.parse(docs) }),
    );
};

await compile("foundation", []);
await Promise.all(
    fs
        .readdirSync("src")
        .filter((dir) => !dir.startsWith(".") && dir !== "foundation")
        .map((dir) => compile(dir, ["foundation"])),
);
