package expressions

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/syntax"
	"wipple/visit"
)

type IsPlaceholder struct{}

func (fact IsPlaceholder) String() string {
	return "is a placeholder"
}

type PlaceholderExpressionNode struct {
	Facts *database.Facts
}

func (node *PlaceholderExpressionNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParsePlaceholderExpression(parser *syntax.Parser) (*PlaceholderExpressionNode, *syntax.Error) {
	span := parser.Spanned()

	_, err := parser.Token("UnderscoreKeyword")
	if err != nil {
		return nil, err
	}

	parser.Commit("in this placeholder expression")

	return &PlaceholderExpressionNode{
		Facts: database.NewFacts(span()),
	}, nil
}

func (node *PlaceholderExpressionNode) Visit(visitor *visit.Visitor) {
	visitExpression(visitor, node)

	database.SetFact(node, IsPlaceholder{})
}

func (node *PlaceholderExpressionNode) Codegen(c *codegen.Codegen) error {
	return c.Error(node)
}
