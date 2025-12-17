package expressions

import (
	"strconv"

	"wipple/codegen"
	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type MissingFieldFact string

func (fact MissingFieldFact) String() string {
	return "is missing field"
}

type ExtraFieldFact string

func (fact ExtraFieldFact) String() string {
	return "is extra field"
}

type DuplicateFieldFact struct{}

func (fact DuplicateFieldFact) String() string {
	return "is duplicate field"
}

type StructureExpressionNode struct {
	Name   string
	Fields []StructureExpressionField
	Facts  *database.Facts
}

func (node *StructureExpressionNode) GetFacts() *database.Facts {
	return node.Facts
}

type StructureExpressionField struct {
	Name  string
	Value database.Node
	Span  database.Span
}

func ParseStructureExpression(parser *syntax.Parser) (*StructureExpressionNode, *syntax.Error) {
	span := parser.Spanned()

	name, err := syntax.ParseTypeName(parser)
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("LeftBrace")
	if err != nil {
		return nil, err
	}

	fields, err := ParseStructureExpressionFields(parser)
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("RightBrace")
	if err != nil {
		return nil, err
	}

	return &StructureExpressionNode{
		Name:   name,
		Fields: fields,
		Facts:  database.NewFacts(span()),
	}, nil
}

func ParseStructureExpressionField(parser *syntax.Parser) (StructureExpressionField, *syntax.Error) {
	span := parser.Spanned()

	name, err := syntax.ParseVariableName(parser)
	if err != nil {
		return StructureExpressionField{}, err
	}

	_, err = parser.Token("AssignOperator")
	if err != nil {
		return StructureExpressionField{}, err
	}

	parser.ConsumeLineBreaks()

	value, err := ParseExpression(parser)
	if err != nil {
		return StructureExpressionField{}, err
	}

	return StructureExpressionField{
		Name:  name,
		Value: value,
		Span:  span(),
	}, nil
}

func ParseStructureExpressionFields(parser *syntax.Parser) ([]StructureExpressionField, *syntax.Error) {
	return syntax.ParseLines(parser, 1, true, ParseStructureExpressionField)
}

func (node *StructureExpressionNode) Visit(visitor *visit.Visitor) {
	visitExpression(visitor, node)

	fields := map[string]database.Node{}
	for _, field := range node.Fields {
		visitor.Visit(field.Value)
		fields[field.Name] = field.Value
	}

	structureConstructorDefinition, ok := visit.Resolve[*visit.StructureConstructorDefinition](visitor, node.Name, node)
	if !ok {
		return
	}

	fieldValues := map[string]database.Node{}
	replacements := map[database.Node]database.Node{structureConstructorDefinition.Node: node}
	for name, field := range structureConstructorDefinition.Fields {
		if _, ok := fields[name]; !ok {
			database.SetFact(node, MissingFieldFact(name))
			continue
		}

		if _, ok := fieldValues[name]; ok {
			database.SetFact(node, DuplicateFieldFact{})
			continue
		}

		fieldValues[name] = field
		replacements[field] = fields[name]
	}

	for name := range fields {
		if _, ok := fieldValues[name]; !ok {
			database.SetFact(node, ExtraFieldFact(name))
		}
	}

	visitor.Constraint(typecheck.InstantiateConstraint(typecheck.Instantiation{
		Source:        node,
		Definition:    structureConstructorDefinition.Node,
		Substitutions: &map[database.Node]typecheck.Type{},
		Replacements:  replacements,
	}, visit.GetDefinitionConstraints))
}

func (node *StructureExpressionNode) Codegen(c *codegen.Codegen) error {
	span := database.GetSpanFact(node)

	c.WriteString(span, "{")
	for _, field := range node.Fields {
		c.WriteString(span, strconv.Quote(field.Name))
		c.WriteString(span, ": ")
		if err := c.Write(field.Value); err != nil {
			return err
		}
		c.WriteString(span, ", ")
	}
	c.WriteString(span, "}")

	return nil
}
