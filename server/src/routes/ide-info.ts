import { handler } from "../handler";
import { InputMetadata, libraries, type Library } from "../library";

const IdeInfoRequest = InputMetadata;

export default handler(IdeInfoRequest, async (body) => {
    if (body.library == null) {
        return { statusCode: 400, body: { error: "missing library" } };
    }

    const info: any[] = [];

    let libraryName: string | undefined = body.library;
    while (libraryName != null) {
        const library: Library | undefined = libraries[libraryName];
        if (library == null) {
            return { statusCode: 400, body: { error: "unknown library" } };
        }

        info.push(library.metadata.ide);

        libraryName = library.metadata.library;
    }

    return { statusCode: 200, body: { info } };
});
