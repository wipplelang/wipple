package patterns

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/syntax"
	"wipple/visit"
)

type VariablePatternNode struct {
	Variable string
	Facts    *database.Facts

	matching database.Node
}

func (node *VariablePatternNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseVariablePattern(parser *syntax.Parser) (*VariablePatternNode, *syntax.Error) {
	span := parser.Spanned()

	name, err := syntax.ParseVariableName(parser)
	if err != nil {
		return nil, err
	}

	return &VariablePatternNode{
		Variable: name,
		Facts:    database.NewFacts(span()),
	}, nil
}

func (node *VariablePatternNode) Visit(visitor *visit.Visitor) {
	visitPattern(visitor, node)
	node.matching = visitor.CurrentMatch.Node

	visitor.Define(node.Variable, &visit.VariableDefinition{
		Name:  node.Variable,
		Node:  node,
		Value: visitor.CurrentMatch.Node,
	})
}

func (node *VariablePatternNode) Codegen(c *codegen.Codegen) error {
	if node.matching == nil {
		return c.Error(node)
	}

	span := database.GetSpanFact(node)

	c.WriteString(span, " && ((")
	c.WriteNode(span, node)
	c.WriteString(span, " = ")
	c.WriteNode(span, node.matching)
	c.WriteString(span, ") || true)")

	return nil
}

func (node *VariablePatternNode) EachTemporary(f func(database.Node)) {
	f(node)
}
