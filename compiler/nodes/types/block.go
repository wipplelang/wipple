package types

import (
	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type BlockTypeNode struct {
	Output database.Node
	Facts  *database.Facts
}

func (node *BlockTypeNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseBlockType(parser *syntax.Parser) (*BlockTypeNode, *syntax.Error) {
	span := parser.Spanned()

	_, err := parser.Token("LeftBrace")
	if err != nil {
		return nil, err
	}

	output, err := ParseTypeElement(parser)
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("RightBrace")
	if err != nil {
		return nil, err
	}

	return &BlockTypeNode{
		Output: output,
		Facts:  database.NewFacts(span()),
	}, nil
}

func (node *BlockTypeNode) Visit(visitor *visit.Visitor) {
	visitType(visitor, node)

	visitor.Visit(node.Output)
	visitor.Constraint(typecheck.TypeConstraint(node, typecheck.BlockType(node.Output)))
}
