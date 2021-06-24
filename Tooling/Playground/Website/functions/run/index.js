const path = require("path");
const util = require("util");
const execFile = util.promisify(require("child_process").execFile);

exports.handler = async (event) => ({
    statusCode: 200,
    body: await run(event.body),
});

const interpreter = path.join(__dirname, "./interpreter");
const run = async (code) => (await execFile(interpreter, [code])).stdout;
