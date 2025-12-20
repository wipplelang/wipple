package expressions

import (
	"fmt"
	"strconv"
	"strings"

	"wipple/codegen"
	"wipple/database"
	"wipple/nodes/types"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type MissingFormatInputsFact int

func (fact MissingFormatInputsFact) String() string {
	return fmt.Sprintf("is missing %d format inputs", fact)
}

type ExtraFormatInputFact struct{}

func (fact ExtraFormatInputFact) String() string {
	return "is extra format input"
}

type FormatExpressionNode struct {
	String string
	Inputs []database.Node
	Facts  *database.Facts

	segments []*formatSegment
	trailing string
}

func (node *FormatExpressionNode) GetFacts() *database.Facts {
	return node.Facts
}

type formatSegment struct {
	string       string
	describeNode database.Node
	input        database.Node
}

func ParseFormatExpression(parser *syntax.Parser) (*FormatExpressionNode, *syntax.Error) {
	span := parser.Spanned()

	stringValue, err := parser.Token("String")
	if err != nil {
		return nil, err
	}

	many, err := syntax.ParseMany(parser, 1, ParseAtomicExpression, syntax.ParseNothing)
	if err != nil {
		return nil, err
	}

	inputs := make([]database.Node, 0, len(many))
	for _, item := range many {
		inputs = append(inputs, item.Value)
	}

	return &FormatExpressionNode{
		String: stringValue,
		Inputs: inputs,
		Facts:  database.NewFacts(span()),
	}, nil
}

func (node *FormatExpressionNode) Visit(visitor *visit.Visitor) {
	visitExpression(visitor, node)

	segmentStrings := strings.Split(node.String, "_")
	if len(segmentStrings) > 0 {
		node.trailing = segmentStrings[len(segmentStrings)-1]
		segmentStrings = segmentStrings[:len(segmentStrings)-1]
	}

	stringType := &types.NamedTypeNode{
		Name:       "String",
		Parameters: nil,
		Facts:      database.NewFacts(database.GetSpanFact(node)),
	}
	visitor.Visit(stringType)

	visitor.Constraint(typecheck.NewGroupConstraint(node, stringType))

	node.segments = make([]*formatSegment, 0, len(segmentStrings))
	missing := make([]string, 0, len(segmentStrings))
	for i, segment := range segmentStrings {
		if i < len(node.Inputs) {
			input := node.Inputs[i]
			visitor.Visit(input)

			node.segments = append(node.segments, &formatSegment{
				string: segment,
				input:  input,
			})
		} else {
			node.segments = append(node.segments, &formatSegment{string: segment})

			missing = append(missing, segment)
		}
	}

	if len(missing) > 0 {
		database.SetFact(node, MissingFormatInputsFact(len(missing)))
	}

	if len(node.Inputs) > len(node.segments) {
		for _, extra := range node.Inputs[len(node.segments):] {
			database.SetFact(extra, ExtraFormatInputFact{})
		}
	}

	for _, segment := range node.segments {
		if segment.input != nil {
			segment.describeNode = &ConstructorExpressionNode{
				ConstructorName: "Describe",
				Facts:           database.NewFacts(database.GetSpanFact(node)),
			}

			visitor.Visit(segment.describeNode)

			visitor.Constraint(typecheck.NewTypeConstraint(segment.describeNode, typecheck.FunctionType[database.Node]([]database.Node{segment.input}, stringType)))
		}
	}
}

func (node *FormatExpressionNode) Codegen(c *codegen.Codegen) error {
	span := database.GetSpanFact(node)

	c.WriteString(span, `(""`)

	for _, segment := range node.segments {
		if segment.describeNode == nil || segment.input == nil {
			return c.Error(node)
		}

		c.WriteString(span, " + ")
		c.WriteString(span, strconv.Quote(segment.string))
		c.WriteString(span, " + ")

		c.WriteString(span, "await (")
		if err := c.Write(segment.describeNode); err != nil {
			return err
		}
		c.WriteString(span, ")(")
		if err := c.Write(segment.input); err != nil {
			return err
		}
		c.WriteString(span, ")")
	}

	c.WriteString(span, " + ")
	c.WriteString(span, strconv.Quote(node.trailing))
	c.WriteString(span, ")")

	return nil
}
