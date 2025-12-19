package expressions

import (
	"reflect"
	"slices"

	"wipple/codegen"
	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type VariableExpressionNode struct {
	Variable string
	Facts    *database.Facts

	resolvedNode          database.Node
	resolvedSubstitutions *map[database.Node]typecheck.Type
}

func (node *VariableExpressionNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseVariableExpression(parser *syntax.Parser) (*VariableExpressionNode, *syntax.Error) {
	span := parser.Spanned()

	name, err := syntax.ParseVariableName(parser)
	if err != nil {
		return nil, err
	}

	return &VariableExpressionNode{
		Variable: name,
		Facts:    database.NewFacts(span()),
	}, nil
}

func (node *VariableExpressionNode) Visit(visitor *visit.Visitor) {
	visitExpression(visitor, node)

	definition, ok := visit.ResolveOf(visitor, node.Variable, node, []reflect.Type{
		reflect.TypeFor[*visit.VariableDefinition](),
		reflect.TypeFor[*visit.ConstantDefinition](),
	})

	if !ok {
		return
	}

	node.resolvedNode = definition.GetNode()

	switch definition := definition.(type) {
	case *visit.VariableDefinition:
		visitor.Constraint(typecheck.NewGroupConstraint(node, definition.Node))
	case *visit.ConstantDefinition:
		node.resolvedSubstitutions = &map[database.Node]typecheck.Type{}

		visitor.Constraint(typecheck.NewInstantiateConstraint(typecheck.Instantiation{
			Source:        node,
			Definition:    definition.Node,
			Substitutions: node.resolvedSubstitutions,
			Replacements:  map[database.Node]database.Node{definition.Node: node},
		}, visit.GetDefinitionConstraints))
	}
}

func (node *VariableExpressionNode) Codegen(c *codegen.Codegen) error {
	if node.resolvedNode == nil {
		return c.Error(node)
	}

	span := database.GetSpanFact(node)

	if node.resolvedSubstitutions == nil {
		c.WriteNode(span, node.resolvedNode)
		return nil
	}

	parameters := make([]database.Node, 0, len(*node.resolvedSubstitutions))
	for parameter := range *node.resolvedSubstitutions {
		parameters = append(parameters, parameter)
	}
	slices.SortStableFunc(parameters, func(left database.Node, right database.Node) int {
		return database.CompareSpans(database.GetSpanFact(left), database.GetSpanFact(right))
	})

	c.WriteString(span, "await runtime.constant(")
	c.WriteNode(span, node.resolvedNode)
	c.WriteString(span, ", types, {")

	for _, parameter := range parameters {
		ty := (*node.resolvedSubstitutions)[parameter]
		c.WriteNode(span, parameter)
		c.WriteString(span, ": ")
		if err := c.WriteType(span, ty); err != nil {
			return err
		}
		c.WriteString(span, ", ")
	}

	c.WriteString(span, "})")

	return nil
}
