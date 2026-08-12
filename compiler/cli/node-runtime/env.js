const env = {
    display: (message) => console.log(message),
    trace: (message) => process.stderr.write(`trace: ${message}\n`),
};

export default env;
