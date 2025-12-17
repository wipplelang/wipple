package patterns

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/nodes/types"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type NumberPatternNode struct {
	Value string
	Facts *database.Facts

	matching database.Node
}

func (node *NumberPatternNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseNumberPattern(parser *syntax.Parser) (*NumberPatternNode, *syntax.Error) {
	span := parser.Spanned()

	value, err := parser.Token("Number")
	if err != nil {
		return nil, err
	}

	return &NumberPatternNode{
		Value: value,
		Facts: database.NewFacts(span()),
	}, nil
}

func (node *NumberPatternNode) Visit(visitor *visit.Visitor) {
	visitPattern(visitor, node)
	node.matching = visitor.CurrentMatch.Node

	numberType := &types.NamedTypeNode{
		Name:       "Number",
		Parameters: []database.Node{},
		Facts:      database.NewFacts(database.GetSpanFact(node)),
	}

	visitor.Visit(numberType)

	visitor.Constraint(typecheck.GroupConstraint(node, numberType))
}

func (node *NumberPatternNode) Codegen(c *codegen.Codegen) error {
	span := database.GetSpanFact(node)

	c.WriteString(span, " && (")
	c.WriteNode(span, node.matching)
	c.WriteString(span, "=== ")
	c.WriteString(span, node.Value)
	c.WriteString(span, ")")

	return nil
}
