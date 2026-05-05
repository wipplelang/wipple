import { handler } from "../handler";
import { InputMetadata, loadLibrary } from "../library";

const DocumentationRequest = InputMetadata;

export default handler(DocumentationRequest, async (body) => {
    if (body.library == null) {
        return { statusCode: 400, body: { error: "missing library" } };
    }

    const result = loadLibrary(body.library);

    if (result == null) {
        throw new Error("compilation failed");
    }

    const items = result.documentation();

    return { statusCode: 200, body: { items } };
});
