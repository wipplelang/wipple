import nextMDX from "@next/mdx";

const withMDX = nextMDX({
    extension: /\.mdx?$/,
});

const config = {
    webpack: (config) => {
        config.module.rules.push({
            test: /\.ya?ml$/,
            use: "yaml-loader",
        });

        return config;
    },
    pageExtensions: ["ts", "tsx", "js", "jsx", "md", "mdx"],
    reactStrictMode: true,
};

export default withMDX(config);
