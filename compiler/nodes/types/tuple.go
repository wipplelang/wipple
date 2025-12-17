package types

import (
	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type TupleTypeNode struct {
	Elements []database.Node
	Facts    *database.Facts
}

func (node *TupleTypeNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseTupleType(parser *syntax.Parser) (*TupleTypeNode, *syntax.Error) {
	span := parser.Spanned()

	many, err := syntax.ParseMany(parser, 1, ParseTypeElement, func(parser *syntax.Parser) (*struct{}, *syntax.Error) {
		_, err := parser.Token("TupleOperator")
		if err != nil {
			return nil, err
		}

		parser.ConsumeLineBreaks()

		return &struct{}{}, nil
	})
	if err != nil {
		return nil, err
	}

	if len(many) == 1 {
		_, err = parser.Token("TupleOperator")
		if err != nil {
			return nil, err
		}
	} else {
		_, _, err := syntax.ParseOptional(parser, func(parser *syntax.Parser) (struct{}, *syntax.Error) {
			_, err := parser.Token("TupleOperator")
			if err != nil {
				return struct{}{}, err
			}

			return struct{}{}, nil
		})
		if err != nil {
			return nil, err
		}
	}

	elements := make([]database.Node, 0, len(many))
	for _, item := range many {
		elements = append(elements, item.Value)
	}

	return &TupleTypeNode{
		Elements: elements,
		Facts:    database.NewFacts(span()),
	}, nil
}

func (node *TupleTypeNode) Visit(visitor *visit.Visitor) {
	visitType(visitor, node)

	for _, element := range node.Elements {
		visitor.Visit(element)
	}

	visitor.Constraint(typecheck.NewTypeConstraint(node, typecheck.TupleType(node.Elements)))
}
