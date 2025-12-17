package expressions

import (
	"strconv"

	"wipple/codegen"
	"wipple/database"
	"wipple/syntax"
	"wipple/visit"
)

type IntrinsicExpressionNode struct {
	Name   string
	Inputs []database.Node
	Facts  *database.Facts
}

func (node *IntrinsicExpressionNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseIntrinsicExpression(parser *syntax.Parser) (*IntrinsicExpressionNode, *syntax.Error) {
	span := parser.Spanned()

	_, err := parser.Token("IntrinsicKeyword", syntax.TokenConfig{
		Commit: "in this `intrinsic` expression",
	})
	if err != nil {
		return nil, err
	}

	name, err := parser.Token("String")
	if err != nil {
		return nil, err
	}

	inputs, ok, err := syntax.ParseOptional(parser, func(parser *syntax.Parser) ([]syntax.Many[database.Node, struct{}], *syntax.Error) {
		return syntax.ParseMany(parser, 0, ParseAtomicExpression, syntax.ParseNothing)
	})
	if err != nil {
		return nil, err
	}

	var nodes []database.Node
	if ok {
		nodes = make([]database.Node, 0, len(inputs))
		for _, input := range inputs {
			nodes = append(nodes, input.Value)
		}
	}

	return &IntrinsicExpressionNode{
		Name:   name,
		Inputs: nodes,
		Facts:  database.NewFacts(span()),
	}, nil
}

func (node *IntrinsicExpressionNode) Visit(visitor *visit.Visitor) {
	visitExpression(visitor, node)

	for _, input := range node.Inputs {
		visitor.Visit(input)
	}
}

func (node *IntrinsicExpressionNode) Codegen(c *codegen.Codegen) error {
	span := database.GetSpanFact(node)

	c.WriteString(span, "await runtime[")
	c.WriteString(span, strconv.Quote(node.Name))
	c.WriteString(span, "](")

	for _, input := range node.Inputs {
		if err := c.Write(input); err != nil {
			return err
		}

		c.WriteString(span, ", ")
	}

	c.WriteString(span, ")")

	return nil
}
