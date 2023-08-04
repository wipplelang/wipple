import * as fs from "fs";
import * as path from "path";
import * as yaml from "js-yaml";
import * as chokidar from "chokidar";
import * as execa from "execa";

const command = process.argv[2];

let running = false;
const run = () => {
    if (running) return;

    running = true;
    execa.execaCommandSync(command, { shell: true, stdio: "inherit" });
    running = false;
};

const taskfile = yaml.load(fs.readFileSync("Taskfile.yml", "utf8"));

const replaceVars = (s) => s.replaceAll(/\{\{.*\}\}/g, "*");

let files = [];
for (const task of Object.values(taskfile.tasks)) {
    const prefix = replaceVars(task.dir ?? "");
    for (const file of task.sources ?? []) {
        files.push(path.join(prefix, replaceVars(file)));
    }
}

files = [...new Set(files)];

console.error("running", command);
run();

chokidar.watch(files, { ignored: ["website/_site/**/*"] }).on("change", (file) => {
    console.error(file, "changed");
    run();
});
