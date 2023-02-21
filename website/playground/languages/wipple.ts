import prism from "prismjs";

export const register = () => {
    prism.languages.wipple = {
        keyword: {
            pattern: /_|use|when|type|trait|instance|where|external|syntax|format/,
            greedy: true,
        },
        operator: { pattern: /:|::|->|=>|\.|\|/, greedy: true },
        type: { pattern: /\b[A-Z][^\r\n\t \(\)\[\]\{\}'"/]*\b/, greedy: true },

        // Standard syntax highlighting rules
        punctuation: { pattern: /\(|\)|\[\[|\]\]|\[|\]|\{|\}|_/, greedy: true },
        whitespace: { pattern: /\s+/, greedy: true },
        comment: { pattern: /--.*/, greedy: true },
        number: { pattern: /-?[0-9]+(\.[0-9]+)?/, greedy: true },
        text: { pattern: /"[^"\\]*(\\.[^"\\]*)*"/s, greedy: true },
        name: { pattern: /[^\n\t \(\)\[\]\{\}"]+/, greedy: true },
        error: /.*/,
    };
};
