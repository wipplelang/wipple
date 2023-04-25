const { Octokit } = require("@octokit/rest");

const headers = {
    "Access-Control-Allow-Origin": "*",
};

exports.handler = async (event, context) => {
    const oktokit = new Octokit();

    const [_leadingSlack, _gist, gistId, file] = event.path.split("/");

    const gist = await oktokit.rest.gists.get({ gist_id: gistId });

    const content = gist.data.files[file]?.content;
    if (!content) {
        return {
            statusCode: 404,
            headers,
            body: "Gist not found",
        };
    }

    return {
        statusCode: 200,
        headers,
        body: content,
    };
};
