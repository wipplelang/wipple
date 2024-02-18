import { LRLanguage, LanguageSupport } from "@codemirror/language";
import { styleTags, tags as t } from "@lezer/highlight";
import { parser } from "./wipple.grammar";

export const wippleTags = {
    Comment: t.comment,
    Text: t.string,
    Number: t.number,
    Keyword: t.keyword,
    Operator: t.operator,
    Type: t.typeName,
};

export const wippleLanguage = (highlight: boolean) =>
    new LanguageSupport(
        LRLanguage.define({
            name: "Wipple",
            parser: parser.configure({
                props: highlight ? [styleTags(wippleTags)] : [],
            }),
            languageData: {
                commentTokens: {
                    block: {
                        open: "[",
                        close: "]",
                    },
                },
                wordChars: "-!?",
                closeBrackets: {
                    brackets: ["(", "[", "{", '"'],
                },
            },
        }),
    );
