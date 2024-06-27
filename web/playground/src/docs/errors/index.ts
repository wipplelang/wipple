import Fuse from "fuse.js";
import * as expectedCodeAfterColonDoc from "./expected-code-after-colon.md";
import * as unknownTypeDoc from "./unknown-type.md";

export interface ErrorDoc {
    error: string;
    doc: string;
}

export const docForError = (message: string): ErrorDoc | undefined => {
    const fuse = new Fuse(docs, { keys: ["attributes.error"] });

    const item = fuse.search(message)[0]?.item;
    if (!item) return undefined;

    return {
        error: item.attributes.error as string,
        doc: item.markdown,
    };
};

const docs = [expectedCodeAfterColonDoc, unknownTypeDoc];
