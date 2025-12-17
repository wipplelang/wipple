package types

import (
	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type UnitTypeNode struct {
	Facts *database.Facts
}

func (node *UnitTypeNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseUnitType(parser *syntax.Parser) (*UnitTypeNode, *syntax.Error) {
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

	return &UnitTypeNode{
		Facts: database.NewFacts(span()),
	}, nil
}

func (node *UnitTypeNode) Visit(visitor *visit.Visitor) {
	visitType(visitor, node)

	visitor.Constraint(typecheck.TypeConstraint(node, typecheck.TupleType([]typecheck.Type{})))
}
