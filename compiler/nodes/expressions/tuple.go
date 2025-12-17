package expressions

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type TupleExpressionNode struct {
	Elements []database.Node
	Facts    *database.Facts
}

func (node *TupleExpressionNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseTupleExpression(parser *syntax.Parser) (*TupleExpressionNode, *syntax.Error) {
	span := parser.Spanned()

	many, err := syntax.ParseMany(parser, 1, ParseExpressionElement, func(parser *syntax.Parser) (*struct{}, *syntax.Error) {
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

	return &TupleExpressionNode{
		Elements: elements,
		Facts:    database.NewFacts(span()),
	}, nil
}

func (node *TupleExpressionNode) Visit(visitor *visit.Visitor) {
	visitExpression(visitor, node)

	for _, element := range node.Elements {
		visitor.Visit(element)
	}

	visitor.Constraint(typecheck.NewTypeConstraint(node, typecheck.TupleType(node.Elements)))
}

func (node *TupleExpressionNode) Codegen(c *codegen.Codegen) error {
	span := database.GetSpanFact(node)

	c.WriteString(span, "[")
	for _, element := range node.Elements {
		if err := c.Write(element); err != nil {
			return err
		}

		c.WriteString(span, ", ")
	}
	c.WriteString(span, "]")

	return nil
}
