package patterns

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type UnitPatternNode struct {
	Facts *database.Facts

	matching database.Node
}

func (node *UnitPatternNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseUnitPattern(parser *syntax.Parser) (*UnitPatternNode, *syntax.Error) {
	span := parser.Spanned()

	_, err := parser.Token("LeftParenthesis", syntax.TokenConfig{
		Reason: "between these parentheses",
	})
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("RightParenthesis")
	if err != nil {
		return nil, err
	}

	return &UnitPatternNode{
		Facts: database.NewFacts(span()),
	}, nil
}

func (node *UnitPatternNode) Visit(visitor *visit.Visitor) {
	visitPattern(visitor, node)
	node.matching = visitor.CurrentMatch.Node

	visitor.Constraint(typecheck.TypeConstraint(node, typecheck.TupleType([]typecheck.Type{})))
}

func (node *UnitPatternNode) Codegen(c *codegen.Codegen) error {
	// No code needed
	return nil
}
