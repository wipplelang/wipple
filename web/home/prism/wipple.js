module.exports = {
    string: /"(?:[^"\\]|\\.)*"/,
    comment: /--.*/,
    keyword: /_|!|when|where|type|trait|instance|intrinsic|infer|do|default/,
    operator: /as|to|by|\^|\*|\/|%|\+|-|<|<=|>|>=|=|\/=|is|and|or|\.|->|:|::|=>/,
    "class-name": /\b[A-Z][^\r\n\t \(\)\[\]\{\}'"/]*\b/,
    number: /\b-?[0-9]+(\.[0-9]+)?\b/,
    whitespace: /\s+/,
    name: /\b.+\b/,
    error: /.*/,
};
