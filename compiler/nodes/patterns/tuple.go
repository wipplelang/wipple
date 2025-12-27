package patterns

import (
	"fmt"

	"wipple/codegen"
	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type TuplePatternNode struct {
	Elements []database.Node
	Facts    *database.Facts

	matching database.Node

	elementTemporaries []database.Node
}

func (node *TuplePatternNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseTuplePattern(parser *syntax.Parser) (*TuplePatternNode, *syntax.Error) {
	span := parser.Spanned()

	many, err := syntax.ParseMany(parser, 1, ParsePatternElement, func(parser *syntax.Parser) (*struct{}, *syntax.Error) {
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

	return &TuplePatternNode{
		Elements: elements,
		Facts:    database.NewFacts(span()),
	}, nil
}

func (node *TuplePatternNode) Visit(visitor *visit.Visitor) {
	visitPattern(visitor, node)
	node.matching = visitor.CurrentMatch.Node

	node.elementTemporaries = make([]database.Node, 0, len(node.Elements))
	for _, element := range node.Elements {
		node.elementTemporaries = append(node.elementTemporaries, visitor.VisitMatching(element))
		visitor.Db.Graph.Edge(element, node, "element")
	}

	visitor.Constraint(typecheck.NewTypeConstraint(node, typecheck.TupleType(node.elementTemporaries)))
}

func (node *TuplePatternNode) Codegen(c *codegen.Codegen) error {
	if len(node.elementTemporaries) != len(node.Elements) {
		return c.Error(node)
	}

	span := database.GetSpanFact(node)

	for index, element := range node.elementTemporaries {
		c.WriteString(span, " && ((")
		c.WriteNode(span, element)
		c.WriteString(span, " = ")
		c.WriteNode(span, node.matching)
		c.WriteString(span, "[")
		c.WriteString(span, fmt.Sprintf("%d", index))
		c.WriteString(span, "]) || true)")

		if err := c.Write(node.Elements[index]); err != nil {
			return err
		}
	}

	return nil
}

func (node *TuplePatternNode) EachTemporary(f func(database.Node)) {
	for _, temporary := range node.elementTemporaries {
		f(temporary)
	}

	for _, element := range node.Elements {
		EachTemporary(element, f)
	}
}
