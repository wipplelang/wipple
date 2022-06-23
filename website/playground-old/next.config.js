/** @type {import('next').NextConfig} */
module.exports = {
    reactStrictMode: true,
    webpack: (config) => {
        config.experiments = {
            asyncWebAssembly: true,
        };

        //////// PATCH https://github.com/vercel/next.js/issues/22581 ////////
        const ssrPlugin = config.plugins.find((plugin) => plugin instanceof SSRPlugin);
        if (ssrPlugin) patchSsrPlugin(ssrPlugin);

        return config;
    },
};

//////// PATCH https://github.com/vercel/next.js/issues/22581 ////////

const SSRPlugin = require("next/dist/build/webpack/plugins/nextjs-ssr-import").default;
const { dirname, relative, resolve, join } = require("path");

// Unfortunately there isn't an easy way to override the replacement function body, so we
// have to just replace the whole plugin `apply` body.
function patchSsrPlugin(plugin) {
    plugin.apply = function apply(compiler) {
        compiler.hooks.compilation.tap("NextJsSSRImport", (compilation) => {
            compilation.mainTemplate.hooks.requireEnsure.tap("NextJsSSRImport", (code, chunk) => {
                // This is the block that fixes https://github.com/vercel/next.js/issues/22581
                if (!chunk.name) {
                    return;
                }

                // Update to load chunks from our custom chunks directory
                const outputPath = resolve("/");
                const pagePath = join("/", dirname(chunk.name));
                const relativePathToBaseDir = relative(pagePath, outputPath);
                // Make sure even in windows, the path looks like in unix
                // Node.js require system will convert it accordingly
                const relativePathToBaseDirNormalized = relativePathToBaseDir.replace(/\\/g, "/");
                return code
                    .replace('require("./"', `require("${relativePathToBaseDirNormalized}/"`)
                    .replace(
                        "readFile(join(__dirname",
                        `readFile(join(__dirname, "${relativePathToBaseDirNormalized}"`
                    );
            });
        });
    };
}
