/* eslint-disable no-useless-escape */

const withBoundary = (regex) => new RegExp(/(?<!\-|\w)/.source + regex.source + /(?!\-|\w)/.source);

const tokens = {
    comment: {
        regex: /--.*/,
        highlight: "comment",
    },
    text: {
        regex: /"[^"]*"|'[^']*'/,
        highlight: "string",
    },
    number: {
        regex: /(?<![\.\d])[+\-]?(?:[\.\d])+(?!\-)(?![\.\d])/,
        highlight: "number",
    },
    binaryOperator: {
        regex: withBoundary(
            /(?:and|as|to|by|<=|>=|\/=|is|or|\x2d>|\^|\*|\/|%|\+|\x2d|<|>|=|\.)(?!(?:[\.\d]|(?:and|as|to|by|<=|>=|\/=|is|or|\x2d>|\^|\*|\/|%|\+|\x2d|<|>|=|\.)))/,
        ),
        highlight: "operator",
    },
    variadicOperator: {
        regex: withBoundary(
            /(?:;|,)(?!(?:[\.\d]|(?:and|as|to|by|<=|>=|\/=|is|or|\x2d>|\^|\*|\/|%|\+|\x2d|<|>|=|\.)))/,
        ),
        highlight: "operator",
    },
    keywordOperator: {
        regex: withBoundary(
            /(?:where|=>|::|:)(?!(?:[\.\d]|(?:and|as|to|by|<=|>=|\/=|is|or|\x2d>|\^|\*|\/|%|\+|\x2d|<|>|=|\.)))/,
        ),
        highlight: "operator",
    },
    keyword: {
        regex: withBoundary(
            /(?:default|do|error|intrinsic|instance|infer|loop|trait|type|when|set|_)/,
        ),
        highlight: "keyword",
    },
    capitalName: {
        regex: /(?<!(?:\w+|\-))(?:\d-)*[A-Z]\w*(?:\-\w+)*[!?]?/,
        highlight: "type",
    },
    lowercaseName: {
        regex: /\w+(?:\-\w+)*[!?]?/,
        highlight: undefined,
    },
};

export const enableHighlightingBefore = /^ *$|(?:\d|\(|\{)+ *$|: +$/;
export const disableHighlightingAfter = /^[^"']*: /;

export default tokens;

export const tokensRegex = new RegExp(
    Object.entries(tokens)
        .map(([name, { regex }]) => `(?<${name}>${regex.source})`)
        .join("|"),
    "g",
);
