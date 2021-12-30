import fs from "fs";
import path from "path";

export const doc = (name: string) =>
    fs.readFileSync(path.resolve(process.cwd(), "docs", name), "utf8");
