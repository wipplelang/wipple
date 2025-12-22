package patterns

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/nodes/types"
	"wipple/syntax"
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

func ParseAnnotatePattern(parser *syntax.Parser) (*AnnotatePatternNode, *syntax.Error) {
	span := parser.Spanned()

	left, err := ParsePatternElement(parser)
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("AnnotateOperator", syntax.TokenConfig{
		Commit: "in this type annotation",
	})
	if err != nil {
		return nil, err
	}

	parser.ConsumeLineBreaks()

	right, err := types.ParseTypeElement(parser)
	if err != nil {
		return nil, err
	}

	return &AnnotatePatternNode{
		Pattern: left,
		Type:    right,
		Facts:   database.NewFacts(span()),
	}, nil
}

func (node *AnnotatePatternNode) Visit(visitor *visit.Visitor) {
	database.HideNode[*AnnotatePatternNode]()

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
