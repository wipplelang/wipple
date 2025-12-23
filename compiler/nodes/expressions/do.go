package expressions

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type DoExpressionNode struct {
	Input database.Node
	Facts *database.Facts
}

func (node *DoExpressionNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseDoExpression(parser *syntax.Parser) (*DoExpressionNode, *syntax.Error) {
	span := parser.Spanned()

	_, err := parser.Token("DoKeyword")
	if err != nil {
		return nil, err
	}

	parser.Commit("in this `do` expression")

	value, err := ParseAtomicExpression(parser)
	if err != nil {
		return nil, err
	}

	return &DoExpressionNode{
		Input: value,
		Facts: database.NewFacts(span()),
	}, nil
}

func (node *DoExpressionNode) Visit(visitor *visit.Visitor) {
	visitExpression(visitor, node)

	visitor.Visit(node.Input)
	visitor.Constraint(typecheck.NewTypeConstraint(node.Input, typecheck.BlockType(node)))
}

func (node *DoExpressionNode) Codegen(c *codegen.Codegen) error {
	span := database.GetSpanFact(node)

	c.WriteString(span, "await (")
	if err := c.Write(node.Input); err != nil {
		return err
	}
	c.WriteString(span, ")()")

	return nil
}
