package patterns

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type OrPatternNode struct {
	Patterns []database.Node
	Facts    *database.Facts

	matching database.Node
}

func (node *OrPatternNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseOrPattern(parser *syntax.Parser) (*OrPatternNode, *syntax.Error) {
	span := parser.Spanned()

	many, err := syntax.ParseMany(parser, 2, ParsePatternElement, func(parser *syntax.Parser) (*struct{}, *syntax.Error) {
		_, err := parser.Token("OrOperator")
		if err != nil {
			return nil, err
		}

		parser.ConsumeLineBreaks()

		return &struct{}{}, nil
	})
	if err != nil {
		return nil, err
	}

	patterns := make([]database.Node, 0, len(many))
	for _, item := range many {
		patterns = append(patterns, item.Value)
	}

	return &OrPatternNode{
		Patterns: patterns,
		Facts:    database.NewFacts(span()),
	}, nil
}

func (node *OrPatternNode) Visit(visitor *visit.Visitor) {
	visitPattern(visitor, node)
	node.matching = visitor.CurrentMatch.Node

	for _, pattern := range node.Patterns {
		visitor.Visit(pattern)
		visitor.Constraint(typecheck.GroupConstraint(node, pattern))
	}
}

func (node *OrPatternNode) Codegen(c *codegen.Codegen) error {
	span := database.GetSpanFact(node)

	c.WriteString(span, " && (false")
	for _, pattern := range node.Patterns {
		c.WriteString(span, " || (true")
		if err := c.Write(pattern); err != nil {
			return err
		}
		c.WriteString(span, ")")
	}
	c.WriteString(span, ")")

	return nil
}

func (node *OrPatternNode) EachTemporary(f func(database.Node)) {
	for _, pattern := range node.Patterns {
		EachTemporary(pattern, f)
	}
}
