import prism from "prismjs";

export const register = () => {
    prism.languages.wipple = {
        "template-before": { pattern: /\(\*(?=[^\(\)\*]*\*\))/, greedy: true },
        "template-after": { pattern: /(?=[\(\*^\(\)\*]*)\*\)/, greedy: true },
        "template-content": {
            pattern: /(?<=\(\*)[^\(\)\*]*(?=\*\))/,
            greedy: true,
            lookbehind: true,
        },

        keyword: {
            pattern:
                /(?<=\s|^)(?:_|use|when|type|trait|instance|where|external|syntax|format|with)(?=\s|$)/,
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
        asset: { pattern: /`[^`]*`/s, greedy: true },
        name: { pattern: /[^\n\t \(\)\[\]\{\}"]+/, greedy: true },
        error: /.*/,
    };
};
