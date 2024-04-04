import { LRLanguage, LanguageSupport } from "@codemirror/language";
import { styleTags, tags as t } from "@lezer/highlight";
import { parser } from "./wipple.grammar";
import { Text } from "@codemirror/state";

export const wippleTags = {
    Comment: t.comment,
    Text: t.string,
    Number: t.number,
    Keyword: t.keyword,
    Operator: t.operator,
    Type: t.typeName,
};

export const wippleLanguage = new LanguageSupport(
    LRLanguage.define({
        name: "Wipple",
        parser: parser.configure({
            props: [styleTags(wippleTags)],
        }),
        languageData: {
            commentTokens: { line: "--" },
            wordChars: "-?",
            closeBrackets: {
                brackets: ["(", "[", "{", '"'],
            },
            indentOnInput: /^\s*[\}\]\)]$/,
        },
    }),
);

export const drop = ({
    snippet,
    doc,
    position,
    selection,
}: {
    snippet: string;
    doc: Text;
    position: number;
    selection: { from: number; to: number };
}) => {
    if (selection.from !== selection.to && position >= selection.from && position <= selection.to) {
        return snippet.replace("_", doc.sliceString(selection.from, selection.to));
    } else {
        snippet = snippet.replace("_", "...");

        const padding = (s: string) => (s === "" || /\s/.test(s) ? "" : " ");
        const leftPadding = padding(doc.sliceString(position - 1, position));
        const rightPadding = padding(doc.sliceString(position, position + 1));

        return leftPadding + snippet + rightPadding;
    }
};
