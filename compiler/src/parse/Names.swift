func parseTypeName(with parser: Parser) throws(ParseError) -> Substring {
    try parser.token(.capitalName, name: "a type name")
}

func parseConstructorName(with parser: Parser) throws(ParseError) -> Substring {
    try parser.token(.capitalName, name: "a constructor name")
}

func parseVariableName(with parser: Parser) throws(ParseError) -> Substring {
    try parser.token(.lowercaseName, name: "a variable name")
}

func parseTypeParameterName(with parser: Parser) throws(ParseError) -> Substring {
    try parser.token(.lowercaseName, name: "a type parameter name")
}

func parseAttributeName(with parser: Parser) throws(ParseError) -> Substring {
    if let name = try parser.parseOptional({ parser throws(ParseError) in
        try parser.token(.lowercaseName, name: "an attribute name")
    }) {
        return name
    }

    if let intrinsic = try parser.parseOptional({ parser throws(ParseError) in
        try parser.token(.intrinsicKeyword)
    }) {
        return intrinsic
    }

    throw parser.error("Expected an attribute name")
}
