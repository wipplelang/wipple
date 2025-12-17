package types

import (
	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type FunctionTypeNode struct {
	Inputs []database.Node
	Output database.Node
	Facts  *database.Facts
}

func (node *FunctionTypeNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseFunctionType(parser *syntax.Parser) (*FunctionTypeNode, *syntax.Error) {
	span := parser.Spanned()

	inputs, err := ParseFunctionTypeInputs(parser)
	if err != nil {
		return nil, err
	}

	output, err := ParseType(parser)
	if err != nil {
		return nil, err
	}

	return &FunctionTypeNode{
		Inputs: inputs,
		Output: output,
		Facts:  database.NewFacts(span()),
	}, nil
}

func ParseFunctionTypeInputs(parser *syntax.Parser) ([]database.Node, *syntax.Error) {
	many, err := syntax.ParseMany(parser, 1, ParseAtomicType, syntax.ParseNothing)
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("FunctionOperator", syntax.TokenConfig{
		Commit: "in this function type",
	})
	if err != nil {
		return nil, err
	}

	parser.ConsumeLineBreaks()

	inputs := make([]database.Node, 0, len(many))
	for _, item := range many {
		inputs = append(inputs, item.Value)
	}

	return inputs, nil
}

func (node *FunctionTypeNode) Visit(visitor *visit.Visitor) {
	visitType(visitor, node)

	for _, input := range node.Inputs {
		visitor.Visit(input)
	}

	visitor.Visit(node.Output)

	visitor.Constraint(typecheck.NewTypeConstraint(node, typecheck.FunctionType(node.Inputs, node.Output)))
}
