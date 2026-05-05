public struct ParseError: Error, Fact {
    public var message: String
    public var reason: String?
    public var committed: String?
    public var span: Span

    public func render(into context: RenderContext) {
        context.string(self.message)
        if let committed = self.committed { context.string(" \(committed)") }
    }
}

public class Parser {
    private class StackEntry { var trace: String? }

    final class CacheToken: Sendable, Hashable {
        static func == (lhs: CacheToken, rhs: CacheToken) -> Bool {
            ObjectIdentifier(lhs) == ObjectIdentifier(rhs)
        }

        func hash(into hasher: inout Hasher) { hasher.combine(ObjectIdentifier(self)) }
    }

    private struct CacheEntry {
        var index: Int
        var value: Any
    }

    let source: String
    private var tokens: [Token] = []
    private var index = 0
    private var stack: [StackEntry] = []
    private var cache: [CacheToken: [Int: CacheEntry]] = [:]

    public init(path: String, source: String) throws(ParseError) {
        self.source = source
        self.tokens = try tokenize(path: path, source: source)
    }

    func spanned() -> () -> Span {
        let start = self.span(at: self.index)

        return {
            let end = self.span(at: self.index == 0 ? 0 : self.index - 1)
            return start.joined(with: end, in: self.source)
        }
    }

    func error(_ message: String, reason: String? = nil) -> ParseError {
        let span = self.span(at: self.index)
        return ParseError(message: message, reason: reason, committed: nil, span: span)
    }

    @discardableResult func token(
        _ kind: Token.Kind,
        name: String? = nil,
        expected: String? = nil,
        reason: String? = nil,
    ) throws(ParseError) -> Substring {
        try self.token(name: name ?? kind.name, expected: expected, reason: reason) { $0 == kind }
    }

    func token(
        name: String,
        expected: String? = nil,
        reason: String? = nil,
        matching matchKind: (Token.Kind) -> Bool,
    ) throws(ParseError) -> Substring {
        let expected = expected ?? name

        guard self.index < self.tokens.count else {
            throw self.error("Expected \(expected)", reason: reason)
        }

        let token = self.tokens[self.index]

        guard matchKind(token.kind) else {
            throw self.error("Expected \(expected), but found \(token.kind)", reason: reason)
        }

        self.index += 1

        return token.value
    }

    func commit(trace: String) { self.stack.last!.trace = trace }

    func consumeLineBreaks() { _ = try? self.token(.lineBreak) }

    func finish() throws(ParseError) {
        guard self.index >= self.tokens.count else {
            let token = self.tokens[self.index]
            throw self.error("Unexpected \(token.kind.name)")
        }
    }

    private func backtrack(to index: Int) { self.index = index }

    private func span(at index: Int) -> Span {
        guard index < self.tokens.count else { return self.eofSpan }
        return self.tokens[index].span
    }

    private var eofSpan: Span { self.tokens.last?.span ?? .empty }
}

extension Parser {
    func parseNothing() throws(ParseError) {}

    func parseCached<T>(token: CacheToken, _ parse: () throws(ParseError) -> T) throws(ParseError)
        -> T
    {
        let start = self.index

        if let cached = self.cache[token, default: [:]][start] {
            self.index = cached.index
            return cached.value as! T
        }

        let result = try parse()

        self.cache[token, default: [:]][start] = CacheEntry(index: self.index, value: result)

        return result
    }

    func parseOptional<T>(_ parse: (Parser) throws(ParseError) -> T) throws(ParseError) -> T? {
        let start = self.index

        let entry = StackEntry()
        self.stack.append(entry)
        defer { self.stack.removeLast() }

        do { return try parse(self) } catch {
            var error = error
            if let trace = entry.trace { error.committed = trace }

            guard error.committed == nil else { throw error }

            self.backtrack(to: start)

            return nil
        }
    }

    func parseMany<T>(min: Int = 0, _ parse: (Parser) throws(ParseError) -> T) throws(ParseError)
        -> [T]
    {
        var lastSpan: Span?
        var results: [T] = []
        while true {
            let start = self.index

            let span = self.spanned()

            guard let result = try self.parseOptional(parse) else {
                self.backtrack(to: start)
                break
            }

            lastSpan = span()
            results.append(result)

        }

        guard results.count >= min else {
            let span = lastSpan ?? self.eofSpan
            throw ParseError(message: "Expected at least \(min) items", span: span)
        }

        return results
    }

    func parseMany<T, S>(
        min: Int = 0,
        _ parse: (Parser) throws(ParseError) -> T,
        separator: (Parser) throws(ParseError) -> S,
    ) throws(ParseError) -> [(T, (S?, Span))] {
        var first = true

        var results: [(T, (S?, Span))] = []
        while true {
            let start = self.index

            let span = self.spanned()

            var separatorResult: S? = nil
            if !first {
                if let result = try self.parseOptional(separator) {
                    separatorResult = result
                } else {
                    break
                }
            }

            let separatorSpan = span()

            guard let result = try self.parseOptional(parse) else {
                self.backtrack(to: start)
                break
            }

            results.append((result, (separatorResult, separatorSpan)))

            first = false
        }

        guard results.count >= min else {
            let span = results.last?.1.1 ?? self.eofSpan
            throw ParseError(message: "Expected at least \(min) items", span: span)
        }

        return results
    }

    func parseLines<T>(
        min: Int = 0,
        requireLineBreaks: Bool,
        _ parse: (Parser) throws(ParseError) -> T,
    ) throws(ParseError) -> [T] {
        self.consumeLineBreaks()

        let result = try self.parseMany(min: min, parse) { parser throws(ParseError) in
            if requireLineBreaks {
                try parser.token(.lineBreak)
            } else {
                parser.consumeLineBreaks()
            }
        }

        self.consumeLineBreaks()

        return result.map(\.0)
    }
}
