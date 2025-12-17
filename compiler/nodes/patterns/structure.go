package patterns

import (
	"strconv"

	"wipple/codegen"
	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type StructurePatternNode struct {
	Name   string
	Fields []StructurePatternField
	Facts  *database.Facts

	matching database.Node

	fieldTemporaries map[string]fieldTemporary
}

type fieldTemporary struct {
	temporary database.Node
	pattern   database.Node
}

func (node *StructurePatternNode) GetFacts() *database.Facts {
	return node.Facts
}

type StructurePatternField struct {
	Name    string
	Pattern database.Node
	Span    database.Span
}

func ParseStructurePattern(parser *syntax.Parser) (*StructurePatternNode, *syntax.Error) {
	span := parser.Spanned()

	name, err := syntax.ParseTypeName(parser)
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("LeftBrace")
	if err != nil {
		return nil, err
	}

	fields, err := syntax.ParseLines(parser, 1, true, ParseStructurePatternField)
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("RightBrace")
	if err != nil {
		return nil, err
	}

	return &StructurePatternNode{
		Name:   name,
		Fields: fields,
		Facts:  database.NewFacts(span()),
	}, nil
}

func ParseStructurePatternField(parser *syntax.Parser) (StructurePatternField, *syntax.Error) {
	span := parser.Spanned()

	name, err := syntax.ParseVariableName(parser)
	if err != nil {
		return StructurePatternField{}, err
	}

	_, err = parser.Token("AssignOperator")
	if err != nil {
		return StructurePatternField{}, err
	}

	parser.ConsumeLineBreaks()

	value, err := ParsePattern(parser)
	if err != nil {
		return StructurePatternField{}, err
	}

	return StructurePatternField{
		Name:    name,
		Pattern: value,
		Span:    span(),
	}, nil
}

func (node *StructurePatternNode) Visit(visitor *visit.Visitor) {
	visitPattern(visitor, node)
	node.matching = visitor.CurrentMatch.Node

	definition, ok := visit.Resolve[*visit.StructureConstructorDefinition](visitor, node.Name, node)
	if !ok {
		return
	}

	node.fieldTemporaries = map[string]fieldTemporary{}
	for _, field := range node.Fields {
		node.fieldTemporaries[field.Name] = fieldTemporary{
			temporary: visitor.VisitMatching(field.Pattern),
			pattern:   field.Pattern,
		}
	}

	replacements := map[database.Node]database.Node{definition.Node: node}
	for fieldName, fieldType := range definition.Fields {
		value, ok := node.fieldTemporaries[fieldName]
		if ok {
			replacements[fieldType] = value.temporary
		}
	}

	visitor.Constraint(typecheck.InstantiateConstraint(typecheck.Instantiation{
		Source:        node,
		Definition:    definition.Node,
		Substitutions: &map[database.Node]typecheck.Type{},
		Replacements:  replacements,
	}, visit.GetDefinitionConstraints))
}

func (node *StructurePatternNode) Codegen(c *codegen.Codegen) error {
	if node.fieldTemporaries == nil {
		return c.Error(node)
	}

	span := database.GetSpanFact(node)

	for name, field := range node.fieldTemporaries {
		c.WriteString(span, " && ((")
		c.WriteNode(span, field.temporary)
		c.WriteString(span, " = ")
		c.WriteNode(span, node.matching)
		c.WriteString(span, "[")
		c.WriteString(span, strconv.Quote(name))
		c.WriteString(span, "]) || true)")

		if err := c.Write(field.pattern); err != nil {
			return err
		}
	}

	return nil
}

func (node *StructurePatternNode) EachTemporary(f func(database.Node)) {
	for _, field := range node.fieldTemporaries {
		f(field.temporary)
		EachTemporary(field.pattern, f)
	}
}
