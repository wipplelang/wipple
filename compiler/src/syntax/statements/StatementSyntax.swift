private let statementCacheToken = Parser.CacheToken()
private let typeRepresentationCacheToken = Parser.CacheToken()

public func parseStatements(with parser: Parser) throws(ParseError) -> [any Visitable] {
    try parser.parseLines(requireLineBreaks: true) { parser throws(ParseError) in
        let statement = try parseStatement(with: parser)
        _ = try parser.parseOptional(parseComment)
        return statement
    }
}

public func parseStatement(with parser: Parser) throws(ParseError) -> any Visitable {
    try parser.parseCached(token: statementCacheToken) { () throws(ParseError) in
        if let value = try parser.parseOptional(parseTypeDefinitionStatement) { return value }
        if let value = try parser.parseOptional(parseTraitDefinitionStatement) { return value }
        if let value = try parser.parseOptional(parseConstantDefinitionStatement) { return value }
        if let value = try parser.parseOptional(parseInstanceDefinitionStatement) { return value }
        if let value = try parser.parseOptional(parseAssignmentStatement) { return value }
        if let value = try parser.parseOptional(parseExpressionStatement) { return value }
        throw parser.error("Expected statement")
    }
}

public func parseComments(with parser: Parser) throws(ParseError) -> [Substring] {
    try parser.parseLines(requireLineBreaks: true, parseComment)
}

public func parseComment(with parser: Parser) throws(ParseError) -> Substring {
    try parser.token(.comment)
}

public func parseTypeRepresentation(with parser: Parser) throws(ParseError) -> any Visitable {
    try parser.parseCached(token: typeRepresentationCacheToken) { () throws(ParseError) in
        if let value = try parser.parseOptional(parseStructureTypeRepresentation) { return value }
        if let value = try parser.parseOptional(parseEnumerationTypeRepresentation) { return value }
        if let value = try parser.parseOptional(parseMarkerTypeRepresentation) { return value }
        throw parser.error("Expected type representation")
    }
}
