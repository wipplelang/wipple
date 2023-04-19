import fs from "fs";
import fm from "front-matter";

let body = fs.readFileSync(0, "utf8");
const sections = [];
do {
    const parsed = fm(body);

    const index = parsed.body.indexOf("---");
    const value = parsed.body.slice(0, index).trim();

    sections.push({
        ...parsed.attributes,
        value,
    });

    if (index === -1) {
        break;
    }

    body = parsed.body.slice(index);
} while (body);

const options = sections.shift();
delete options.value;

console.log(JSON.stringify({ ...options, sections }, null, 4));
