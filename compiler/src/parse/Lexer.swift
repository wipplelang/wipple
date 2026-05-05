import RegexBuilder

public struct Token {
    public let kind: Kind
    public let span: Span
    public let value: Substring

    public enum Kind {
        case lineBreak
        case comment
        case typeFunctionOperator
        case annotateOperator
        case assignOperator
        case functionOperator
        case lessThanOrEqualOperator
        case greaterThanOrEqualOperator
        case notEqualOperator
        case powerOperator
        case multiplyOperator
        case divideOperator
        case remainderOperator
        case addOperator
        case subtractOperator
        case lessThanOperator
        case greaterThanOperator
        case equalOperator
        case applyOperator
        case tupleOperator
        case collectionOperator
        case leftParenthesis
        case rightParenthesis
        case leftBracket
        case rightBracket
        case leftBrace
        case rightBrace
        case underscoreKeyword
        case doKeyword
        case inferKeyword
        case instanceKeyword
        case intrinsicKeyword
        case setKeyword
        case traitKeyword
        case typeKeyword
        case whenKeyword
        case whereKeyword
        case asOperator
        case toOperator
        case byOperator
        case isOperator
        case andOperator
        case orOperator
        case number
        case string
        case capitalName
        case lowercaseName
    }
}

func tokenize(path: String, source: String) throws(ParseError) -> [Token] {
    let tokenRegexes: [(Regex<(Substring, Substring)>, Token.Kind?)] = [
        (/([ \t]+)/, nil),  //
        (/(\n+)/, .lineBreak),  //
        (/--([^\n]*)/, .comment),  //
        (/(=>)/, .typeFunctionOperator),  //
        (/(::)/, .annotateOperator),  //
        (/(:)/, .assignOperator),  //
        (/(->)/, .functionOperator),  //
        (/(<=)/, .lessThanOrEqualOperator),  //
        (/(>=)/, .greaterThanOrEqualOperator),  //
        (/(\/=)/, .notEqualOperator),  //
        (/(\^)/, .powerOperator),  //
        (/(\*)/, .multiplyOperator),  //
        (/(\/)/, .divideOperator),  //
        (/(%)/, .remainderOperator),  //
        (/(\+)/, .addOperator),  //
        (/(-)/, .subtractOperator),  //
        (/(<)/, .lessThanOperator),  //
        (/(>)/, .greaterThanOperator),  //
        (/(=)/, .equalOperator),  //
        (/(\.)/, .applyOperator),  //
        (/(;)/, .tupleOperator),  //
        (/(,)/, .collectionOperator),  //
        (/(\()/, .leftParenthesis),  //
        (/(\))/, .rightParenthesis),  //
        (/(\[)/, .leftBracket),  //
        (/(\])/, .rightBracket),  //
        (/(\{)/, .leftBrace),  //
        (/(\})/, .rightBrace),  //
        (/(_)/, .underscoreKeyword),  //
        (/(do)/, .doKeyword),  //
        (/(infer)/, .inferKeyword),  //
        (/(instance)/, .instanceKeyword),  //
        (/(intrinsic)/, .intrinsicKeyword),  //
        (/(set)/, .setKeyword),  //
        (/(trait)/, .traitKeyword),  //
        (/(type)/, .typeKeyword),  //
        (/(when)/, .whenKeyword),  //
        (/(where)/, .whereKeyword),  //
        (/(as)/, .asOperator),  //
        (/(to)/, .toOperator),  //
        (/(by)/, .byOperator),  //
        (/(is)/, .isOperator),  //
        (/(and)/, .andOperator),  //
        (/(or)/, .orOperator),  //
        (/(\-?\d+(?:\.\d+)?)/, .number),  //
        (/"([^"]*)"/, .string),  //
        (/'([^']*)'/, .string),  //
        (/((?:\d+-)*[A-Z][A-Za-z0-9_]*(?:-[A-Za-z0-9_]+)*(?:[!?])?)/, .capitalName),  //
        (/((?:\d+-)*[A-Za-z0-9_]+(?:-[A-Za-z0-9_]+)*(?:[!?])?)/, .lowercaseName),  //
    ]

    var remaining = Substring(source)
    var tokens: [Token] = []
    var currentLine = 1
    var currentColumn = 1
    while !remaining.isEmpty {
        var longestMatch: (Substring, Substring, Token.Kind?)?
        for (regex, kind) in tokenRegexes {
            if let match = remaining.prefixMatch(of: regex) {
                let (substring, capture) = match.output

                if let (longestSubstring, _, _) = longestMatch,
                    longestSubstring.count >= substring.count
                {
                    continue
                }

                longestMatch = (substring, capture, kind)
            }
        }

        let location = {
            Location(
                line: currentLine,
                column: currentColumn,
                index: source.utf16.distance(from: source.startIndex, to: remaining.startIndex),
            )
        }

        let start = location()

        guard let (substring, value, kind) = longestMatch else {
            throw ParseError(
                message: "Unexpected character",
                reason: nil,
                committed: nil,
                span: Span(path: path, start: start, end: start, in: source),
            )
        }

        remaining = remaining[substring.endIndex...]

        for character in substring {
            if character == "\n" {
                currentLine += 1
                currentColumn = 1
            } else {
                currentColumn += 1
            }
        }

        let end = location()

        let span = Span(path: path, start: start, end: end, in: source)

        if let kind {
            let token = Token(kind: kind, span: span, value: value)
            tokens.append(token)
        }
    }

    return tokens
}

extension Token.Kind {
    var isKeyword: Bool {
        switch self {
        case .doKeyword, .inferKeyword, .instanceKeyword, .intrinsicKeyword, .setKeyword,
            .traitKeyword, .typeKeyword, .underscoreKeyword, .whenKeyword:
            true
        default: false
        }
    }

    var isOperator: Bool {
        switch self {
        case .typeFunctionOperator, .annotateOperator, .assignOperator, .functionOperator,
            .lessThanOrEqualOperator, .greaterThanOrEqualOperator, .notEqualOperator,
            .powerOperator, .multiplyOperator, .divideOperator, .remainderOperator, .addOperator,
            .subtractOperator, .lessThanOperator, .greaterThanOperator, .equalOperator,
            .applyOperator, .tupleOperator, .collectionOperator, .asOperator, .toOperator,
            .byOperator, .isOperator, .andOperator, .orOperator, .whereKeyword:
            true
        default: false
        }
    }

    var isBinaryOperator: Bool {
        self.isOperator && !self.isNonAssociativeOperator && !self.isVariadicOperator
    }

    var isNonAssociativeOperator: Bool {
        switch self {
        case .whereKeyword, .typeFunctionOperator, .annotateOperator, .assignOperator: true
        default: false
        }
    }

    var isVariadicOperator: Bool {
        switch self {
        case .tupleOperator, .collectionOperator: true
        default: false
        }
    }

    var isOpening: Bool {
        switch self {
        case .leftParenthesis, .leftBracket, .leftBrace: true
        default: false
        }
    }

    var isClosing: Bool {
        switch self {
        case .rightParenthesis, .rightBracket, .rightBrace: true
        default: false
        }
    }
}

extension Token.Kind {
    public var name: String {
        switch self {
        case .lineBreak: "a line break"
        case .comment: "a comment"
        case .typeFunctionOperator: "`=>`"
        case .annotateOperator: "`::`"
        case .assignOperator: "`:`"
        case .functionOperator: "`->`"
        case .lessThanOrEqualOperator: "`<=`"
        case .greaterThanOrEqualOperator: "`>=`"
        case .notEqualOperator: "`/=`"
        case .powerOperator: "`^`"
        case .multiplyOperator: "`*`"
        case .divideOperator: "`/`"
        case .remainderOperator: "`%`"
        case .addOperator: "`+`"
        case .subtractOperator: "`-`"
        case .lessThanOperator: "`<`"
        case .greaterThanOperator: "`>`"
        case .equalOperator: "`=`"
        case .applyOperator: "`.`"
        case .tupleOperator: "`;`"
        case .collectionOperator: "`,`"
        case .leftParenthesis: "`(`"
        case .rightParenthesis: "`)`"
        case .leftBracket: "`[`"
        case .rightBracket: "`]`"
        case .leftBrace: "`{`"
        case .rightBrace: "`}`"
        case .underscoreKeyword: "`_`"
        case .doKeyword: "`do`"
        case .inferKeyword: "`infer`"
        case .instanceKeyword: "`instance`"
        case .intrinsicKeyword: "`intrinsic`"
        case .setKeyword: "`set`"
        case .traitKeyword: "`trait`"
        case .typeKeyword: "`type`"
        case .whenKeyword: "`when`"
        case .whereKeyword: "`where`"
        case .asOperator: "`as`"
        case .toOperator: "`to`"
        case .byOperator: "`by`"
        case .isOperator: "`is`"
        case .andOperator: "`and`"
        case .orOperator: "`or`"
        case .number: "a number"
        case .string: "a string"
        case .capitalName: "a capital name"
        case .lowercaseName: "a lowercase name"
        }
    }
}
