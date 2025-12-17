package patterns

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/typecheck"
	"wipple/visit"
)

type AnnotatePatternNode struct {
	Pattern database.Node
	Type    database.Node
	Facts   *database.Facts

	matching database.Node
}

func (node *AnnotatePatternNode) GetFacts() *database.Facts {
	return node.Facts
}

func (node *AnnotatePatternNode) Visit(visitor *visit.Visitor) {
	visitPattern(visitor, node)
	node.matching = visitor.CurrentMatch.Node

	visitor.Visit(node.Pattern)
	visitor.Visit(node.Type)

	visitor.Constraint(typecheck.NewGroupConstraint(node.Pattern, node.Type))
	visitor.Constraint(typecheck.NewGroupConstraint(node, node.Pattern))
}

func (node *AnnotatePatternNode) Codegen(c *codegen.Codegen) error {
	return c.Write(node.Pattern)
}

func (node *AnnotatePatternNode) EachTemporary(f func(database.Node)) {
	EachTemporary(node.Pattern, f)
}
