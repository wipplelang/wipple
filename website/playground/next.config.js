/** @type {import('next').NextConfig} */
module.exports = {
    reactStrictMode: false,
    webpack: (config) => {
        config.experiments = {
            asyncWebAssembly: true,
        };

        return config;
    },
};
