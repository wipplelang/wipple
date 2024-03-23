const config = {
    testRegex: ".*\\.test\\.wipple$",
    moduleFileExtensions: ["js", "wipple"],
    extensionsToTreatAsEsm: [".wipple"],
    transform: {
        "\\.wipple$": "<rootDir>/wipple-transformer.js",
    },
    moduleNameMapper: {
        "\\.wipple$": "$1",
    },
};

export default config;
