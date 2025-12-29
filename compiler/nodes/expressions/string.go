package expressions

import (
	"strconv"

	"wipple/codegen"
	"wipple/database"
	"wipple/nodes/types"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type StringExpressionNode struct {
	Value string
	Facts *database.Facts
}

func (node *StringExpressionNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseStringExpression(parser *syntax.Parser) (*StringExpressionNode, *syntax.Error) {
	span := parser.Spanned()

	value, err := parser.Token("String")
	if err != nil {
		return nil, err
	}

	return &StringExpressionNode{
		Value: value,
		Facts: database.NewFacts(span()),
	}, nil
}

func (node *StringExpressionNode) Visit(visitor *visit.Visitor) {
	visitExpression(visitor, node)

	stringType := &types.NamedTypeNode{
		Name:       "String",
		Parameters: nil,
		Facts:      database.NewFacts(database.GetSpanFact(node)),
	}
	visitor.Visit(stringType)
	visitor.Db.Graph.Replace(stringType, nil)

	visitor.Constraint(typecheck.NewGroupConstraint(node, stringType))
}

func (node *StringExpressionNode) Codegen(c *codegen.Codegen) error {
	span := database.GetSpanFact(node)
	c.WriteString(span, strconv.Quote(node.Value))
	return nil
}
