/* eslint-disable no-useless-escape */

const withBoundary = (regex: RegExp) =>
    new RegExp(/(?<!\-|\w)/.source + regex.source + /(?!\-|\w)/.source);

const tokens = {
    comment: /--.*/,
    text: /"[^"]*"|'[^']*'/,
    number: /(?<![\.\d])[+\-]?(?:[\.\d])+(?!\-)(?![\.\d])/,
    binaryOperator: withBoundary(
        /(?:and|as|to|by|<=|>=|\/=|is|or|\x2d>|\^|\*|\/|%|\+|\x2d|<|>|=|\.)(?!(?:[\.\d]|(?:and|as|to|by|<=|>=|\/=|is|or|\x2d>|\^|\*|\/|%|\+|\x2d|<|>|=|\.)))/,
    ),
    variadicOperator: withBoundary(
        /(?:;|,)(?!(?:[\.\d]|(?:and|as|to|by|<=|>=|\/=|is|or|\x2d>|\^|\*|\/|%|\+|\x2d|<|>|=|\.)))/,
    ),
    keywordOperator: withBoundary(
        /(?:where|=>|::|:)(?!(?:[\.\d]|(?:and|as|to|by|<=|>=|\/=|is|or|\x2d>|\^|\*|\/|%|\+|\x2d|<|>|=|\.)))/,
    ),
    keyword: withBoundary(/(?:intrinsic|instance|infer|trait|type|when|set|do|_)/),
    capitalName: /(?<!(?:\w+|\-))(?:\d-)*[A-Z]\w*(?:\-\w+)*[!?]?/,
    lowercaseName: /\w+(?:\-\w+)*[!?]?/,
};

export const enableHighlightingBefore = /^ *$|(?:\d|\(|\{)+ *$|: +$/;
export const disableHighlightingAfter = /^[^"']*: /;

export default tokens;

export const tokensRegex = new RegExp(
    Object.entries(tokens)
        .map(([name, regex]) => `(?<${name}>${regex.source})`)
        .join("|"),
    "g",
);
