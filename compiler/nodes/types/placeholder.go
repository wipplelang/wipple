package types

import (
	"wipple/database"
	"wipple/syntax"
	"wipple/visit"
)

type PlaceholderTypeNode struct {
	Facts *database.Facts
}

func (node *PlaceholderTypeNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParsePlaceholderType(parser *syntax.Parser) (*PlaceholderTypeNode, *syntax.Error) {
	span := parser.Spanned()

	_, err := parser.Token("UnderscoreKeyword")
	if err != nil {
		return nil, err
	}

	parser.Commit("in this placeholder type")

	return &PlaceholderTypeNode{
		Facts: database.NewFacts(span()),
	}, nil
}

func (node *PlaceholderTypeNode) Visit(visitor *visit.Visitor) {
	visitType(visitor, node)
}
