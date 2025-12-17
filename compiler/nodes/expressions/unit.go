package expressions

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type UnitExpressionNode struct {
	Facts *database.Facts
}

func (node *UnitExpressionNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseUnitExpression(parser *syntax.Parser) (*UnitExpressionNode, *syntax.Error) {
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

	return &UnitExpressionNode{
		Facts: database.NewFacts(span()),
	}, nil
}

func (node *UnitExpressionNode) Visit(visitor *visit.Visitor) {
	visitExpression(visitor, node)

	visitor.Constraint(typecheck.TypeConstraint(node, typecheck.TupleType([]database.Node{})))
}

func (node *UnitExpressionNode) Codegen(c *codegen.Codegen) error {
	span := database.GetSpanFact(node)
	c.WriteString(span, "[]")
	return nil
}
