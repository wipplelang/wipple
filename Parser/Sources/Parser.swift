import Foundation
import Wipple

public struct ParseError: Swift.Error {
    public let message: String
    public let location: SourceLocation?

    public init(_ message: String, _ location: SourceLocation? = nil) {
        self.message = message
        self.location = location
    }
}

public struct AST: Hashable {
    public var node: Node
    public var location: SourceLocation

    public init(_ node: Node, _ location: SourceLocation) {
        self.node = node
        self.location = location
    }

    public indirect enum Node: Hashable {
        case block([Statement])
        case list([AST])
        case name(String)
        case number(Decimal)
        case text(String)
        case literal(AST)
        case escaped(AST)

        public struct Statement: Hashable {
            public var items: [AST]
            public var location: SourceLocation
        }
    }
}

public typealias TokenIterator = _PeekableIterator<Array<TokenWithLocation>.Iterator>

public extension Array where Element == TokenWithLocation {
    func forParsing() -> TokenIterator {
        _PeekableIterator(
            lazy
                .filter { token in
                    switch token.token {
                    case .whitespace, .comment:
                        return false
                    default:
                        return true
                    }
                }
                .makeIterator()
        )
    }
}

public func parseBlock(_ tokens: TokenIterator) throws -> AST? {
    guard case let (.openBrace, start)? = tokens.peek()?.tuple else {
        return nil
    }

    tokens.next()

    let statements = try parseStatements(tokens)

    parseNewlines(tokens)

    if let (token, end) = tokens.next()?.tuple {
        guard case .closeBrace = token else {
            throw ParseError("Expected '}'", end)
        }

        return AST(.block(statements), start)
    }

    throw ParseError("Expected '}'")
}

public func parseFile(_ tokens: TokenIterator) throws -> AST {
    let start: SourceLocation
    if let (_, location) = tokens.peek()?.tuple {
        start = SourceLocation(file: location.file, line: 1, column: 1)
    } else {
        start = SourceLocation(line: 1, column: 1)
    }

    let statements = try parseStatements(tokens)

    parseNewlines(tokens)

    if let (_, location) = tokens.next()?.tuple {
        throw ParseError("Unexpected token", location)
    }

    return AST(.block(statements), start)
}

public func parseInlineProgram(_ tokens: TokenIterator) throws -> AST {
    let start: SourceLocation
    if let (_, location) = tokens.peek()?.tuple {
        start = SourceLocation(file: location.file, line: 1, column: 1)
    } else {
        start = SourceLocation(line: 1, column: 1)
    }

    var values: [AST] = []

    while let value = try parseValue(tokens) {
        values.append(value)
    }

    if let (_, location) = tokens.next()?.tuple {
        throw ParseError("Unexpected token", location)
    }

    return AST(.list(values), start)
}

private func parseStatements(_ tokens: TokenIterator) throws -> [AST.Node.Statement] {
    var statements: [(items: [AST], location: SourceLocation?)] = [([], nil)]

    while true {
        if parseNewline(tokens) {
            statements.append(([], nil))
            continue
        }

        parseNewlines(tokens)

        guard let value = try parseValue(tokens) else {
            break
        }

        let index = statements.endIndex - 1

        statements[index].items.append(value)

        if statements[index].location == nil {
            statements[index].location = value.location
        }
    }

    return statements.compactMap {
        let (items, location) = $0
        return location.map { .init(items: items, location: $0) }
    }
}

public func parseList(_ tokens: TokenIterator) throws -> AST? {
    guard case let (.openParenthesis, start)? = tokens.peek()?.tuple else {
        return nil
    }

    tokens.next()

    var items: [AST] = []

    while true {
        parseNewlines(tokens)

        guard let value = try parseValue(tokens) else {
            break
        }

        items.append(value)
    }

    if let (token, end) = tokens.next()?.tuple {
        guard case .closeParenthesis = token else {
            throw ParseError("Expected ')'", end)
        }

        return AST(.list(items), start)
    }

    throw ParseError("Expected ')'")
}

public func parseBrackets(_ tokens: TokenIterator) throws -> AST? {
    if case let (.openBracket, start)? = tokens.peek()?.tuple {
        throw ParseError("Bracket syntax is reserved", start)
    }

    return nil
}

public func parseName(_ tokens: TokenIterator) throws -> AST? {
    guard case let (.name(name), start)? = tokens.peek()?.tuple else {
        return nil
    }

    tokens.next()

    return AST(.name(name), start)
}

public func parseNumber(_ tokens: TokenIterator) throws -> AST? {
    guard case let (.number(number), start)? = tokens.peek()?.tuple else {
        return nil
    }

    tokens.next()

    return AST(.number(number), start)
}

public func parseText(_ tokens: TokenIterator) throws -> AST? {
    guard case let (.text(text), start)? = tokens.peek()?.tuple else {
        return nil
    }

    tokens.next()

    return AST(.text(text), start)
}

public func parseLiteral(_ tokens: TokenIterator) throws -> AST? {
    guard case let (.quote, start)? = tokens.peek()?.tuple else {
        return nil
    }

    tokens.next()

    guard let value = try parseValue(tokens) else {
        throw ParseError("Expected value", start)
    }

    return AST(.literal(value), start)
}

public func parseEscaped(_ tokens: TokenIterator) throws -> AST? {
    guard case let (.backslash, start)? = tokens.peek()?.tuple else {
        return nil
    }

    tokens.next()

    guard let value = try parseValue(tokens) else {
        throw ParseError("Expected value", start)
    }

    return AST(.escaped(value), start)
}

private func parseValue(_ tokens: TokenIterator) throws -> AST? {
    let choices = [
        parseBlock,
        parseList,
        parseBrackets,
        parseName,
        parseText,
        parseNumber,
        parseLiteral,
        parseEscaped,
    ]

    for choice in choices {
        if let value = try choice(tokens) {
            return value
        }
    }

    return nil
}

private func parseNewline(_ tokens: TokenIterator) -> Bool {
    guard case (.newline, _)? = tokens.peek()?.tuple else {
        return false
    }

    tokens.next()

    return true
}

private func parseNewlines(_ tokens: TokenIterator) {
    while parseNewline(tokens) {}
}

public class _PeekableIterator<Base: IteratorProtocol>: IteratorProtocol {
    private var peeked: Base.Element?
    private var iter: Base

    public init(_ base: Base) {
        iter = base
        peeked = iter.next()
    }

    public func peek() -> Base.Element? {
        peeked
    }

    @discardableResult
    public func next() -> Base.Element? {
        let result = peeked

        if peeked != nil {
            peeked = iter.next()
        }

        return result
    }
}
