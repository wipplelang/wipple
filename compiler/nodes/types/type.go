package types

import (
	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type IsTypeFact struct{}

func (fact IsTypeFact) String() string {
	return "is a type"
}

type MissingTypesFact []database.Node

func (fact MissingTypesFact) String() string {
	return "is missing type"
}

type ExtraTypeFact struct{}

func (fact ExtraTypeFact) String() string {
	return "is extra type"
}

func ParseType(parser *syntax.Parser) (database.Node, *syntax.Error) {
	return syntax.ParseCached(parser, func(p *syntax.Parser) (database.Node, *syntax.Error) {
		tupleType, ok, err := syntax.ParseOptional(parser, ParseTupleType)
		if err != nil {
			return nil, err
		}
		if ok {
			return tupleType, nil
		}

		functionType, ok, err := syntax.ParseOptional(parser, ParseFunctionType)
		if err != nil {
			return nil, err
		}
		if ok {
			return functionType, nil
		}

		parameterType, ok, err := syntax.ParseOptional(parser, ParseAnnotatedParameterType)
		if err != nil {
			return nil, err
		}
		if ok {
			return parameterType, nil
		}

		typeElement, ok, err := syntax.ParseOptional(parser, ParseTypeElement)
		if err != nil {
			return nil, err
		}
		if ok {
			return typeElement, nil
		}

		return nil, parser.Error("Expected type")
	})
}

func ParseTypeElement(parser *syntax.Parser) (database.Node, *syntax.Error) {
	return syntax.ParseCached(parser, func(p *syntax.Parser) (database.Node, *syntax.Error) {
		parameterizedType, ok, err := syntax.ParseOptional(parser, ParseParameterizedType)
		if err != nil {
			return nil, err
		}
		if ok {
			return parameterizedType, nil
		}

		atomicType, ok, err := syntax.ParseOptional(parser, ParseAtomicType)
		if err != nil {
			return nil, err
		}
		if ok {
			return atomicType, nil
		}

		return nil, parser.Error("Expected type")
	})
}

func ParseAtomicType(parser *syntax.Parser) (database.Node, *syntax.Error) {
	return syntax.ParseCached(parser, func(p *syntax.Parser) (database.Node, *syntax.Error) {
		placeholderType, ok, err := syntax.ParseOptional(parser, ParsePlaceholderType)
		if err != nil {
			return nil, err
		}
		if ok {
			return placeholderType, nil
		}

		parameterType, ok, err := syntax.ParseOptional(parser, ParseParameterType)
		if err != nil {
			return nil, err
		}
		if ok {
			return parameterType, nil
		}

		namedType, ok, err := syntax.ParseOptional(parser, ParseNamedType)
		if err != nil {
			return nil, err
		}
		if ok {
			return namedType, nil
		}

		blockType, ok, err := syntax.ParseOptional(parser, ParseBlockType)
		if err != nil {
			return nil, err
		}
		if ok {
			return blockType, nil
		}

		unitType, ok, err := syntax.ParseOptional(parser, ParseUnitType)
		if err != nil {
			return nil, err
		}
		if ok {
			return unitType, nil
		}

		parenthesizedType, ok, err := syntax.ParseOptional(parser, ParseParenthesizedType)
		if err != nil {
			return nil, err
		}
		if ok {
			return parenthesizedType, nil
		}

		return nil, parser.Error("Expected type")
	})
}

func ParseParenthesizedType(parser *syntax.Parser) (database.Node, *syntax.Error) {
	span := parser.Spanned()

	_, err := parser.Token("LeftParenthesis", syntax.TokenConfig{
		Reason: "between these parentheses",
	})
	if err != nil {
		return nil, err
	}

	parser.ConsumeLineBreaks()

	value, err := ParseType(parser)
	if err != nil {
		return nil, err
	}

	parser.ConsumeLineBreaks()

	_, err = parser.Token("RightParenthesis")
	if err != nil {
		return nil, err
	}

	database.SetSpanFact(value, span())

	return value, nil
}

func visitType(visitor *visit.Visitor, node database.Node) {
	database.SetFact(node, IsTypeFact{})
	database.SetFact(node, typecheck.TypedFact{})
}
