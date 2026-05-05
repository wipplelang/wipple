import DequeModule

public func format(_ code: String) -> String? {
    // The formatted code is stored here.
    var formatted = ""

    // Rather than using an iterator, a VecDeque allows us to perform lookahead
    // without consuming tokens. This is done when determining whether to reset
    // the indentation of a line.
    guard var tokens = try? Deque(tokenize(path: "", source: code)) else { return nil }

    // We track indentation within groups of parentheses, brackets and braces
    // using a stack. Whenever we encounted an opening grouping symbol, we push
    // `true` (indicating the symbol comes immediately before a new line) or
    // `false` (it doesn't) to the stack. Closing grouping symbols pop from the
    // stack.
    var groups: [Bool] = []

    enum LineIndent {
        case group
        case trailingOperator
    }

    // Similarly, we track what caused a line to be indented. This is used to
    // reset the line indent only as far as needed. For example...
    //
    // ```wipple
    // {
    //   f :
    //     x ->
    //       42
    //
    //  f ()
    // }
    // ```
    //
    // ...would cause `LineIndent::Group` to be pushed once, and then
    // `LineIndent::TrailingOperator` to be pushed twice (for the definition of
    // `f`). Once the definition of `f` is over, we want to remove the
    // `TrailingOperator` indents, but not the `Group` indent, since we want the
    // call to `f` to remain indented within the block.
    //
    // You'll see this pattern several times below -- we remove
    // `TrailingOperator` indents until we encounter a `Group` indent, and then
    // remove a single `Group` indent.
    var lineIndents: [LineIndent] = []

    // This tracks whether to insert a space before a token. We add padding
    // after non-grouping symbols like names and numbers, but skip padding after
    // parentheses, brackets and braces. Padding is also only respected for non-
    // grouping symbols; we want code like `( x )` to be formatted as `(x)`, not
    // `(x )`.
    var pad = true

    // This tracks whether the previous token was an opening grouping symbol for
    // the purpose of inserting a space before a comment. Even though we don't
    // insert a space in the case of `(x`, we always insert a space before
    // comments (ie. `( -- comment`). When we insert a comment, we check if
    // either `pad` or `first_in_group` are set.
    var firstInGroup = false

    // Iterate through the tokens, writing them to `formatted` one at a time.
    while let token = tokens.popFirst() {
        switch token.kind {
        case .leftParenthesis:
            if pad { formatted.append(" ") }

            formatted.append("(")

            pad = false
            firstInGroup = true

            if tokens.first?.kind == .lineBreak {
                lineIndents.append(.group)
                groups.append(true)
            } else {
                groups.append(false)
            }
        case .rightParenthesis:
            formatted.append(")")

            pad = true
            firstInGroup = false

            if groups.popLast() ?? false {
                while lineIndents.last == .trailingOperator { _ = lineIndents.popLast() }
                if lineIndents.last == .group { _ = lineIndents.popLast() }
            }
        case .leftBracket:
            if pad { formatted.append(" ") }

            formatted.append("[")

            pad = false
            firstInGroup = true

            if tokens.first?.kind == .lineBreak {
                lineIndents.append(.group)
                groups.append(true)
            } else {
                groups.append(false)
            }
        case .rightBracket:
            formatted.append("]")

            pad = true
            firstInGroup = false

            if groups.popLast() ?? false {
                while lineIndents.last == .trailingOperator { _ = lineIndents.popLast() }
                if lineIndents.last == .group { _ = lineIndents.popLast() }
            }
        case .leftBrace:
            if pad { formatted.append(" ") }

            formatted.append("{")

            pad = false
            firstInGroup = true

            if tokens.first?.kind == .lineBreak {
                lineIndents.append(.group)
                groups.append(true)
            } else {
                groups.append(false)
            }
        case .rightBrace:
            formatted.append("}")

            pad = true
            firstInGroup = false

            if groups.popLast() ?? false {
                while lineIndents.last == .trailingOperator { _ = lineIndents.popLast() }
                if lineIndents.last == .group { _ = lineIndents.popLast() }
            }
        case .lineBreak:
            // Collapse multiple line breaks into at most two line breaks.
            // For example:
            //
            // ```wipple
            // foo
            //
            //
            // bar
            // ```
            //
            // Is formatted as:
            //
            // ```wipple
            // foo
            //
            // bar
            // ```
            let multiple = token.value.count > 1

            // As long as we encounter closing grouping symbols before any
            // other tokens, we need to remove indentation. That way, code
            // like this:
            //
            // ```wipple
            // {
            //   x
            // }
            // ```
            //
            // Isn't formatted as:
            //
            // ```wipple
            // {
            //   x
            //   }
            // ```
            //
            // We store the closing symbols in a queue and write them to
            // `formatted` after removing indentation.
            var queue = ""

            // This is used to insert leading indentation if the first token
            // is an operator.
            var leadingIndent = false

            outer: while let token = tokens.first {
                let character: Character
                switch token.kind {
                case .rightParenthesis: character = ")"
                case .rightBracket: character = "]"
                case .rightBrace: character = "}"
                case let kind where kind.isOperator:
                    // Only set the leading indent if the operator is
                    // the first token on the line. Otherwise, code
                    // like:
                    //
                    // ```wipple
                    // {
                    // } -> {}
                    // ```
                    //
                    // Would insert a leading indent because of the
                    // `->` and be formatted as:
                    //
                    // ```wipple
                    // {
                    //   } -> {}
                    // ```
                    leadingIndent = queue.isEmpty
                    break outer
                default:
                    // Stop on any other token and so it can be printed
                    // as normal.
                    break outer
                }

                _ = tokens.popFirst()
                queue.append(character)

                // Make sure to handle the standard resetting of
                // grouping/indentation, since here, the most recent token
                // was a closing grouping symbol.
                if groups.popLast() ?? false {
                    while lineIndents.last == .trailingOperator { _ = lineIndents.popLast() }
                    if lineIndents.last == .group { _ = lineIndents.popLast() }
                }
            }

            // Add a single extra new line if there are multiple line
            // breaks.
            if multiple { formatted.append("\n") }

            var indent = lineIndents.count
            if leadingIndent { indent += 1 }

            formatted.append("\n")
            for _ in 0..<indent {
                // Wipple uses 2-space indents.
                formatted.append("  ")
            }

            formatted.append(queue)

            pad = !queue.isEmpty
            firstInGroup = false

            // Fially, we determine whether to reset the trailing operator
            // indent. We do this by looking ahead to the end of the next
            // line. The rules for resetting indentation are:
            //
            //   -  If the last token (on the following line) is an opening
            //      grouping symbol or operator, don't reset. In fact, we
            //      indent another level.
            //
            //   -  Any other token (eg. a name) causes a reset because
            //      the operator expression is over.
            var reset = !queue.isEmpty
            for token in tokens {
                if token.kind == .lineBreak { break }

                reset = !token.kind.isOpening && !token.kind.isOperator
            }

            if reset { while lineIndents.last == .trailingOperator { _ = lineIndents.popLast() } }
        case .comment:
            if pad || firstInGroup { formatted.append(" ") }

            formatted.append("--")
            formatted.append(contentsOf: token.value)
        case let kind where kind.isKeyword:
            if pad { formatted.append(" ") }

            formatted.append(contentsOf: token.value)
            pad = true
            firstInGroup = false
        case let kind where kind.isBinaryOperator:
            if pad { formatted.append(" ") }

            formatted.append(contentsOf: token.value)

            pad = true
            firstInGroup = false

            if tokens.first?.kind == .lineBreak { lineIndents.append(.trailingOperator) }
        case let kind where kind.isVariadicOperator:
            formatted.append(contentsOf: token.value)

            pad = true
            firstInGroup = false

            if tokens.first?.kind == .lineBreak { lineIndents.append(.trailingOperator) }
        case let kind where kind.isNonAssociativeOperator:
            if pad { formatted.append(" ") }

            formatted.append(contentsOf: token.value)

            pad = true
            firstInGroup = false

            if tokens.first?.kind == .lineBreak { lineIndents.append(.trailingOperator) }
        case .lowercaseName, .capitalName:
            if pad { formatted.append(" ") }

            formatted.append(contentsOf: token.value)
            pad = true
            firstInGroup = false
        case .string:
            if pad { formatted.append(" ") }

            let quote = token.value.contains("\"") ? "'" : "\""

            formatted.append(quote)
            formatted.append(contentsOf: token.value)
            formatted.append(quote)
            pad = true
            firstInGroup = false
        case .number:
            if pad { formatted.append(" ") }

            formatted.append(contentsOf: token.value)

            pad = true
            firstInGroup = false
        default: fatalError("unknown token: \(token)")
        }
    }

    return formatted.trimmingCharacters(in: .whitespacesAndNewlines)
}
