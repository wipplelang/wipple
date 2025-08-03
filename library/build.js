const path = require("node:path");
const fs = require("node:fs");
const process = require("node:process");

process.chdir(__dirname);

fs.rmSync("dist", { recursive: true, force: true });
fs.mkdirSync("dist", { recursive: true });

for (let dir of fs.readdirSync("src")) {
    if (!fs.statSync(path.join("src", dir)).isDirectory()) {
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

    fs.writeFileSync(path.join("dist", `${dir}.json`), JSON.stringify({ metadata, files }));
}
