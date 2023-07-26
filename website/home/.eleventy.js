module.exports = (eleventy) => {
    eleventy.setTemplateFormats(["html", "md", "jpg", "png", "svg", "webp", "ico"]);

    eleventy.addPlugin(require("@11ty/eleventy-plugin-syntaxhighlight"), {
        init: ({ Prism }) => {
            Prism.languages.wipple = require("./prism/wipple");
        },
    });

    eleventy.addPassthroughCopy("_redirects");
    eleventy.addPassthroughCopy("robots.txt");
    eleventy.addPassthroughCopy("styles/prism.css");

    return {
        dir: {
            input: ".",
            output: "../_site",
            includes: "includes",
            layouts: "layouts",
        },
        htmlTemplateEngine: "njk",
    };
};
