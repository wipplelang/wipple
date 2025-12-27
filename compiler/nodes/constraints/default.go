package constraints

import (
	"wipple/database"
	"wipple/nodes/types"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type DefaultConstraintNode struct {
	Parameter database.Node
	Value     database.Node
	Facts     *database.Facts
}

func (node *DefaultConstraintNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseDefaultConstraint(parser *syntax.Parser) (*DefaultConstraintNode, *syntax.Error) {
	span := parser.Spanned()

	_, err := parser.Token("LeftParenthesis", syntax.TokenConfig{
		Reason: "between these parentheses",
	})
	if err != nil {
		return nil, err
	}

	parameterSpan := parser.Spanned()

	name, err := syntax.ParseTypeParameterName(parser)
	if err != nil {
		return nil, err
	}

	parameter := &types.TypeParameterNode{
		Name:  name,
		Infer: false,
		Value: nil,
		Facts: database.NewFacts(parameterSpan()),
	}

	_, err = parser.Token("AnnotateOperator")
	if err != nil {
		return nil, err
	}

	parser.Commit("in this type annotation")

	parser.ConsumeLineBreaks()

	value, err := types.ParseType(parser)
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("RightParenthesis")
	if err != nil {
		return nil, err
	}

	return &DefaultConstraintNode{
		Parameter: parameter,
		Value:     value,
		Facts:     database.NewFacts(span()),
	}, nil
}

func (node *DefaultConstraintNode) Visit(visitor *visit.Visitor) {
	visitConstraint(visitor, node)

	visitor.Visit(node.Parameter)
	visitor.Db.Graph.Edge(node.Parameter, node, "parameter")
	visitor.Visit(node.Value)
	visitor.Db.Graph.Edge(node.Value, node, "value")
	visitor.Constraint(typecheck.NewDefaultConstraint(node.Parameter, node.Value))
}
