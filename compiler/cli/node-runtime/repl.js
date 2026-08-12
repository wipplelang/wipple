import http from "node:http";

const port = process.argv.at(-1);
if (!port) {
    throw new Error("missing port");
}

const server = http.createServer((req, res) => {
    let body = "";
    req.on("data", (chunk) => {
        body += chunk;
    });

    req.on("end", () => {
        try {
            eval(body);
        } catch (e) {
            console.error(e);
        } finally {
            res.writeHead(200, { "Content-Type": "text/plain" });
            res.write("OK");
            res.end();
        }
    });
});

server.listen(port);
