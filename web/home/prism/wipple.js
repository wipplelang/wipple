module.exports = {
    string: /"(?:[^"\\]|\\.)*"/,
    comment: /--.*/,
    number: /-?[0-9]+(?:\.[0-9]+)?/,
    keyword: /\b(?:_|when|where|type|trait|instance|intrinsic|infer|do|default)\b/,
    operator:
        /@|!|\bas\b|\bto\b|\bby\b|\^|\*|\/|%|\+|-|<|<=|>|>=|=|\/=|\bis\b|\band\b|\bor\b|\.|->|:|::|=>/,
    "class-name": /\b(?:[A-Z][A-Za-z0-9\-_]*[?]?)\b/,
    name: /\b(?:[A-Za-z0-9\-_]+[?]?)\b/,
};
