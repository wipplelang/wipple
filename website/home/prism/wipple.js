module.exports = {
    string: /"(?:[^"\\]|\\.)*"/,
    comment: /--.*/,
    keyword: /_|use|when|type|trait|instance|where|external/,
    operator: /:|::|->|=>|~>|\.|\|/,
    "class-name": /\b[A-Z][^\r\n\t \(\)\[\]\{\}'"/]*\b/,
    number: /\b-?[0-9]+(\.[0-9]+)?\b/,
    punctuation: /['\/]/,
    whitespace: /\s+/,
    name: /\b.+\b/,
    error: /.*/,
};
