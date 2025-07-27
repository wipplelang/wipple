import fs from "node:fs";
import runtime from "./runtime/runtime.js";

let src;
if (process.argv[2] === "compile") {
    const code = fs.readFileSync("test.wipple", "utf8");

    const response = await fetch("http://localhost:3000", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ compile: { library: "foundation", code } }),
    }).then((response) => response.json());

    if (response.success) {
        fs.writeFileSync("out.js", response.executable);

        src = `data:text/javascript,${encodeURIComponent(response.executable)}`;
    } else {
        console.error(response.diagnostics);
        process.exit(1);
    }
} else if (process.argv[2] === "run") {
    src = "./out.js";
} else {
    throw new Error("expected 'compile' or 'run'");
}

const { default: entrypoint } = await import(src);

const env = {
    display: console.log,
    debug: (value) => console.error("--- debug:", value),
};

await entrypoint(runtime(env));
