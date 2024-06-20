const { format } = require("date-fns");

module.exports = (eleventy) => {
    eleventy.setTemplateFormats(["html", "md", "jpg", "png", "svg", "webp", "ico", "gif"]);

    eleventy.addPlugin(require("@jamshop/eleventy-plugin-typography"));

    eleventy.addPlugin(require("@11ty/eleventy-plugin-syntaxhighlight"), {
        init: ({ Prism }) => {
            Prism.languages.wipple = require("./prism/wipple");
        },
    });

    eleventy.addPassthroughCopy({ firebase: "__" });
    eleventy.addPassthroughCopy("_headers");
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
