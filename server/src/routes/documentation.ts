import { handler } from "../handler";
import { InputMetadata, libraries } from "../library";

const DocumentationRequest = InputMetadata;

export default handler(DocumentationRequest, async (body) => {
    if (body.library == null) {
        return { statusCode: 400, body: { error: "missing library" } };
    }

    const library = libraries[body.library]?.[0];
    if (library == null) {
        return { statusCode: 400, body: { error: "unknown library" } };
    }

    return { statusCode: 200, body: { items: library.docs } };
});
