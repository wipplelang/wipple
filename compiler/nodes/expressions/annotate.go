package expressions

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/nodes/types"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type AnnotateExpressionNode struct {
	Expression database.Node
	Type       database.Node
	Facts      *database.Facts
}

func (node *AnnotateExpressionNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseAnnotateExpression(parser *syntax.Parser) (*AnnotateExpressionNode, *syntax.Error) {
	span := parser.Spanned()

	left, err := ParseExpressionElement(parser)
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("AnnotateOperator", syntax.TokenConfig{
		Commit: "in this type annotation",
	})
	if err != nil {
		return nil, err
	}

	parser.ConsumeLineBreaks()

	right, err := types.ParseTypeElement(parser)
	if err != nil {
		return nil, err
	}

	return &AnnotateExpressionNode{
		Expression: left,
		Type:       right,
		Facts:      database.NewFacts(span()),
	}, nil
}

func (node *AnnotateExpressionNode) Visit(visitor *visit.Visitor) {
	visitExpression(visitor, node)

	visitor.Visit(node.Expression)
	visitor.Visit(node.Type)

	visitor.Constraint(typecheck.NewGroupConstraint(node.Expression, node.Type))
	visitor.Constraint(typecheck.NewGroupConstraint(node, node.Expression))
}

func (node *AnnotateExpressionNode) Codegen(c *codegen.Codegen) error {
	return c.Write(node.Expression)
}
