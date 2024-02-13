import { LRLanguage, LanguageSupport } from "@codemirror/language";
import { styleTags, tags as t } from "@lezer/highlight";
import { parser } from "./wipple.grammar";

export const wippleLanguage = new LanguageSupport(
    LRLanguage.define({
        name: "Wipple",
        parser: parser.configure({
            props: [
                styleTags({
                    Comment: t.comment,
                    Placeholder: t.name,
                    QuoteName: t.name,
                    RepeatName: t.name,
                    Text: t.string,
                    Number: t.number,
                    Asset: t.string,
                    Keyword: t.keyword,
                    Operator: t.operator,
                    Type: t.typeName,
                    Name: t.name,
                }),
            ],
        }),
        languageData: {
            commentTokens: { block: { open: "[", close: "]" } },
            wordChars: "-!?",
            closeBrackets: {
                brackets: ["(", "[", "{", '"'],
            },
        },
    }),
);
