package expressions

import (
	"reflect"

	"wipple/codegen"
	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type ConstructorExpressionNode struct {
	ConstructorName string
	Facts           *database.Facts

	matchingConstructorDefinition visit.Definition
	matchingSubstitutions         map[database.Node]typecheck.Type
}

func (node *ConstructorExpressionNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseConstructorExpression(parser *syntax.Parser) (*ConstructorExpressionNode, *syntax.Error) {
	span := parser.Spanned()

	name, err := syntax.ParseConstructorName(parser)
	if err != nil {
		return nil, err
	}

	return &ConstructorExpressionNode{
		ConstructorName: name,
		Facts:           database.NewFacts(span()),
	}, nil
}

func (node *ConstructorExpressionNode) Visit(visitor *visit.Visitor) {
	visitExpression(visitor, node)

	constructorDefinition, ok := visit.ResolveOf(visitor, node.ConstructorName, node, []reflect.Type{
		reflect.TypeFor[*visit.TraitDefinition](),
		reflect.TypeFor[*visit.VariantConstructorDefinition](),
		reflect.TypeFor[*visit.MarkerConstructorDefinition](),
	})

	if !ok {
		return
	}

	substitutions := map[database.Node]typecheck.Type{}

	visitor.Constraint(typecheck.NewInstantiateConstraint(typecheck.Instantiation{
		Source:        node,
		Definition:    constructorDefinition.GetNode(),
		Substitutions: &substitutions,
		Replacements:  map[database.Node]database.Node{constructorDefinition.GetNode(): node},
	}, visit.GetDefinitionConstraints))

	if traitDefinition, ok := constructorDefinition.(*visit.TraitDefinition); ok {
		visitor.Constraint(typecheck.NewBoundConstraint(node, typecheck.UnresolvedBound{
			Source:        node,
			Trait:         traitDefinition.Node,
			Substitutions: &substitutions,
			TraitName:     traitDefinition.Name,
		}, visit.GetDefinitionConstraints))

		node.matchingSubstitutions = substitutions
	}

	node.matchingConstructorDefinition = constructorDefinition
}

func (node *ConstructorExpressionNode) Codegen(c *codegen.Codegen) error {
	if node.matchingConstructorDefinition == nil {
		return c.Error(node)
	}

	span := database.GetSpanFact(node)

	switch definition := node.matchingConstructorDefinition.(type) {
	case *visit.MarkerConstructorDefinition:
		c.WriteString(span, "null")
	case *visit.TraitDefinition:
		if node.matchingSubstitutions == nil {
			return c.Error(node)
		}

		c.WriteString(span, "await runtime.trait(")
		c.WriteNode(span, definition.Node)
		c.WriteString(span, ", types, {")

		for parameter, ty := range node.matchingSubstitutions {
			c.WriteNode(span, parameter)
			c.WriteString(span, ": ")
			if err := c.WriteType(span, ty); err != nil {
				return err
			}
			c.WriteString(span, ", ")
		}

		c.WriteString(span, "})")
	case *visit.VariantConstructorDefinition:
		c.Write(definition.Node)
	default:
		return c.Error(node)
	}

	return nil
}
