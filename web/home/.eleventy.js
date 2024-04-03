const { format } = require("date-fns");
const { parse: parseHtml } = require("node-html-parser");

module.exports = (eleventy) => {
    eleventy.setTemplateFormats(["html", "md", "jpg", "png", "svg", "webp", "ico", "gif"]);

    eleventy.addPlugin(require("@jamshop/eleventy-plugin-typography"));
    eleventy.addPlugin(lazyImages, {});

    eleventy.addPlugin(require("@11ty/eleventy-plugin-syntaxhighlight"), {
        init: ({ Prism }) => {
            Prism.languages.wipple = require("./prism/wipple");
        },
    });

    eleventy.addPassthroughCopy("_redirects");
    eleventy.addPassthroughCopy("robots.txt");
    eleventy.addPassthroughCopy("styles/prism.css");

    eleventy.addFilter("postDate", (date) =>
        format(forceDateToLocalTimeZone(date), "MMMM d, yyyy")
    );

    eleventy.setBrowserSyncConfig({ reloadDelay: 1500 });

    return {
        dir: {
            input: ".",
            output: "./_site",
            includes: "_includes",
            layouts: "_layouts",
        },
        htmlTemplateEngine: "njk",
    };
};

// Adapted from https://ryankubik.com/blog/force-date-time-zone
function forceDateToLocalTimeZone(date) {
    // https://stackoverflow.com/a/34405528/5569234
    const timeZone = new Date()
        .toLocaleTimeString("en-us", { timeZoneName: "short" })
        .split(" ")[2];

    const [isoDate] = date.toISOString().split("T");

    return new Date(`${isoDate} ${timeZone}`);
}

// https://cri.dev/posts/2021-05-11-how-to-lazy-load-images-eleventy/
function lazyImages(eleventyConfig, userOptions = {}) {
    const { parse } = require("node-html-parser");

    const options = {
        name: "lazy-images",
        ...userOptions,
    };

    eleventyConfig.addTransform(options.extensions, (content, outputPath) => {
        if (outputPath.endsWith(".html")) {
            const root = parse(content);
            const images = root.querySelectorAll("img");
            images.forEach((img) => {
                img.setAttribute("loading", "lazy");
            });
            return root.toString();
        }
        return content;
    });
}
