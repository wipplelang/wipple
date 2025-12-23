package patterns

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/syntax"
	"wipple/visit"
)

type WildcardPatternNode struct {
	Facts *database.Facts

	matching database.Node
}

func (node *WildcardPatternNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseWildcardPattern(parser *syntax.Parser) (*WildcardPatternNode, *syntax.Error) {
	span := parser.Spanned()

	_, err := parser.Token("UnderscoreKeyword")
	if err != nil {
		return nil, err
	}

	parser.Commit("in this wildcard pattern")

	return &WildcardPatternNode{
		Facts: database.NewFacts(span()),
	}, nil
}

func (node *WildcardPatternNode) Visit(visitor *visit.Visitor) {
	visitPattern(visitor, node)
	node.matching = visitor.CurrentMatch.Node
}

func (node *WildcardPatternNode) Codegen(c *codegen.Codegen) error {
	// No code needed
	return nil
}
