module.exports = (eleventy) => {
    eleventy.setTemplateFormats(["html", "md", "jpg", "png", "svg", "webp", "ico"]);

    eleventy.addPlugin(require("@11ty/eleventy-plugin-syntaxhighlight"), {
        init: ({ Prism }) => {
            Prism.languages.wipple = require("./home/prism/wipple");
        },
    });

    eleventy.addPassthroughCopy("home/robots.txt");
    eleventy.addPassthroughCopy("home/styles/prism.css");

    return {
        dir: {
            input: "home",
            includes: "includes",
            layouts: "layouts",
        },
        htmlTemplateEngine: "njk",
    };
};
