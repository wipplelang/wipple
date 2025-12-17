package expressions

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/nodes/types"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type AsExpressionNode struct {
	Left  database.Node
	Right database.Node
	Facts *database.Facts

	asFunction database.Node
}

func (node *AsExpressionNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseAsExpression(parser *syntax.Parser) (*AsExpressionNode, *syntax.Error) {
	span := parser.Spanned()

	left, err := ParseExpressionElement(parser)
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("AsOperator")
	if err != nil {
		return nil, err
	}

	parser.ConsumeLineBreaks()

	right, err := types.ParseTypeElement(parser)
	if err != nil {
		return nil, err
	}

	return &AsExpressionNode{
		Left:  left,
		Right: right,
		Facts: database.NewFacts(span()),
	}, nil
}

func (node *AsExpressionNode) Visit(visitor *visit.Visitor) {
	visitExpression(visitor, node)

	visitor.Visit(node.Left)
	visitor.Visit(node.Right)

	node.asFunction = &ConstructorExpressionNode{
		ConstructorName: "As",
		Facts:           database.NewFacts(database.GetSpanFact(node)),
	}

	visitor.Visit(node.asFunction)
	visitor.Constraint(typecheck.TypeConstraint(node.asFunction, typecheck.FunctionType([]database.Node{node.Left}, node.Right)))

	visitor.Constraint(typecheck.GroupConstraint(node, node.Right))
}

func (node *AsExpressionNode) Codegen(c *codegen.Codegen) error {
	if node.asFunction == nil {
		return c.Error(node)
	}

	span := database.GetSpanFact(node)

	call := &CallExpressionNode{
		Function: node.asFunction,
		Inputs:   []database.Node{node.Left},
		Facts:    database.NewFacts(span),
	}

	return c.Write(call)
}
