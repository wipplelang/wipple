import main from "./main.js";

main({
    display: (message) => console.log(message),
    trace: (message) => process.stderr.write(`trace: ${message}\n`),
});
