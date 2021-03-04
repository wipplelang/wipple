const CopyWebpackPlugin = require("copy-webpack-plugin");
const path = require("path");

module.exports = {
  entry: "./src/bootstrap.js",
  output: {
    path: path.resolve(__dirname, "build"),
    filename: "bootstrap.js",
  },
  mode: process.env.NODE_ENV || "development",
  plugins: [new CopyWebpackPlugin(["index.html", "assets"])],
};
