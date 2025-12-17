package expressions

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/nodes/types"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type NumberExpressionNode struct {
	Value string
	Facts *database.Facts
}

func (node *NumberExpressionNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseNumberExpression(parser *syntax.Parser) (*NumberExpressionNode, *syntax.Error) {
	span := parser.Spanned()

	value, err := parser.Token("Number")
	if err != nil {
		return nil, err
	}

	return &NumberExpressionNode{
		Value: value,
		Facts: database.NewFacts(span()),
	}, nil
}

func (node *NumberExpressionNode) Visit(visitor *visit.Visitor) {
	visitExpression(visitor, node)

	numberType := &types.NamedTypeNode{
		Name:       "Number",
		Parameters: nil,
		Facts:      database.NewFacts(database.GetSpanFact(node)),
	}
	visitor.Visit(numberType)

	visitor.Constraint(typecheck.GroupConstraint(node, numberType))
}

func (node *NumberExpressionNode) Codegen(c *codegen.Codegen) error {
	span := database.GetSpanFact(node)
	c.WriteString(span, node.Value)
	return nil
}
