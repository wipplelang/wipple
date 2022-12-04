/** @type {import('next').NextConfig} */
module.exports = {
    basePath: "/playground",
    reactStrictMode: false,
    webpack: (config) => {
        config.experiments = {
            asyncWebAssembly: true,
        };

        return config;
    },
};
