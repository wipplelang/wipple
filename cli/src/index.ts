import fs from "fs";
import util from "util";
import { compile, link } from "wipple-compiler";
import { evaluate } from "wipple-interpreter";

const path = process.argv[2];
const code = fs.readFileSync(path, "utf8");

const result = compile([{ path, code }], []);

console.log(util.inspect(result, { depth: Infinity, colors: true }));

const executable = link([result.library]);
console.log(executable);

if (!executable) {
    process.exit(1);
}

console.log(util.inspect(executable, { depth: Infinity, colors: true }));

await evaluate(executable);
