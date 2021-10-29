const paths = ["../../target", "out"];

module.exports = {
    async onPreBuild({ utils }) {
        console.log("Restoring paths:", paths);

        await Promise.all(paths.map(utils.cache.restore));
    },

    async onPostBuild({ utils }) {
        console.log("Caching paths:", paths);

        await Promise.all(paths.map(utils.cache.save));
    },
};
