package patterns

import (
	"strconv"

	"wipple/codegen"
	"wipple/database"
	"wipple/nodes/types"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type StringPatternNode struct {
	Value string
	Facts *database.Facts

	matching database.Node
}

func (node *StringPatternNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseStringPattern(parser *syntax.Parser) (*StringPatternNode, *syntax.Error) {
	span := parser.Spanned()

	value, err := parser.Token("String")
	if err != nil {
		return nil, err
	}

	return &StringPatternNode{
		Value: value,
		Facts: database.NewFacts(span()),
	}, nil
}

func (node *StringPatternNode) Visit(visitor *visit.Visitor) {
	visitPattern(visitor, node)
	node.matching = visitor.CurrentMatch.Node

	stringType := &types.NamedTypeNode{
		Name:       "String",
		Parameters: []database.Node{},
		Facts:      database.NewFacts(database.GetSpanFact(node)),
	}

	visitor.Visit(stringType)

	visitor.Constraint(typecheck.NewGroupConstraint(node, stringType))
}

func (node *StringPatternNode) Codegen(c *codegen.Codegen) error {
	span := database.GetSpanFact(node)

	c.WriteString(span, " && (")
	c.WriteNode(span, node.matching)
	c.WriteString(span, "=== ")
	c.WriteString(span, strconv.Quote(node.Value))
	c.WriteString(span, ")")

	return nil
}
