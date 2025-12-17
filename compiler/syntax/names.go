package syntax

func ParseTypeName(parser *Parser) (string, *Error) {
	return parser.Token("CapitalName", TokenConfig{Name: "a type name"})
}

func ParseConstructorName(parser *Parser) (string, *Error) {
	return parser.Token("CapitalName", TokenConfig{Name: "a constructor name"})
}

func ParseVariableName(parser *Parser) (string, *Error) {
	return parser.Token("LowercaseName", TokenConfig{Name: "a variable name"})
}

func ParseTypeParameterName(parser *Parser) (string, *Error) {
	return parser.Token("LowercaseName", TokenConfig{Name: "a type parameter name"})
}

func ParseAttributeName(parser *Parser) (string, *Error) {
	name, ok, err := ParseOptional(parser, func(parser *Parser) (string, *Error) {
		return parser.Token("LowercaseName", TokenConfig{Name: "an attribute name"})
	})
	if err != nil {
		return "", err
	}
	if ok {
		return name, nil
	}

	intrinsic, ok, err := ParseOptional(parser, func(parser *Parser) (string, *Error) {
		return parser.Token("IntrinsicKeyword")
	})
	if err != nil {
		return "", err
	}
	if ok {
		return intrinsic, nil
	}

	return "", parser.Error("expected an attribute name")
}
