package attributes

import (
	"wipple/database"
	"wipple/syntax"
)

type StringAttributeValueNode struct {
	Value string
	Facts *database.Facts
}

func (node *StringAttributeValueNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseAttributeValue(parser *syntax.Parser) (database.Node, *syntax.Error) {
	return syntax.ParseCached(parser, func(p *syntax.Parser) (database.Node, *syntax.Error) {
		stringValue, ok, err := syntax.ParseOptional(parser, ParseStringAttributeValue)
		if err != nil {
			return nil, err
		}
		if ok {
			return stringValue, nil
		}

		return nil, parser.Error("Expected attribute value")
	})
}

func ParseStringAttributeValue(parser *syntax.Parser) (*StringAttributeValueNode, *syntax.Error) {
	span := parser.Spanned()

	value, err := parser.Token("String")
	if err != nil {
		return nil, err
	}

	return &StringAttributeValueNode{
		Value: value,
		Facts: database.NewFacts(span()),
	}, nil
}
