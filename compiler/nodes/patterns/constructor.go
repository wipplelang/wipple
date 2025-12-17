package patterns

import (
	"fmt"
	"reflect"

	"wipple/codegen"
	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type ExtraElementFact struct{}

func (fact ExtraElementFact) String() string {
	return "is extra element"
}

type ConstructorPatternNode struct {
	ConstructorName string
	Elements        []database.Node
	Facts           *database.Facts

	matching database.Node

	matchingMarkerConstructor  *visit.MarkerConstructorDefinition
	matchingVariantConstructor *matchingVariantConstructor
}

type matchingVariantConstructor struct {
	index    int
	elements []matchingVariantConstructorElement
}

type matchingVariantConstructorElement struct {
	temporary database.Node
	pattern   database.Node
}

func (node *ConstructorPatternNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseParameterizedConstructorPattern(parser *syntax.Parser) (*ConstructorPatternNode, *syntax.Error) {
	span := parser.Spanned()

	constructor, err := syntax.ParseConstructorName(parser)
	if err != nil {
		return nil, err
	}

	many, ok, err := syntax.ParseOptional(parser, func(parser *syntax.Parser) ([]syntax.Many[database.Node, struct{}], *syntax.Error) {
		return syntax.ParseMany(parser, 1, ParseAtomicPattern, syntax.ParseNothing)
	})
	if err != nil {
		return nil, err
	}

	var elements []database.Node
	if ok {
		elements = make([]database.Node, 0, len(many))
		for _, item := range many {
			elements = append(elements, item.Value)
		}
	}

	return &ConstructorPatternNode{
		ConstructorName: constructor,
		Elements:        elements,
		Facts:           database.NewFacts(span()),
	}, nil
}

func ParseConstructorPattern(parser *syntax.Parser) (*ConstructorPatternNode, *syntax.Error) {
	span := parser.Spanned()

	constructor, err := syntax.ParseConstructorName(parser)
	if err != nil {
		return nil, err
	}

	return &ConstructorPatternNode{
		ConstructorName: constructor,
		Elements:        nil,
		Facts:           database.NewFacts(span()),
	}, nil
}

func (node *ConstructorPatternNode) Visit(visitor *visit.Visitor) {
	visitPattern(visitor, node)
	node.matching = visitor.CurrentMatch.Node

	definition, ok := visit.ResolveOf(visitor, node.ConstructorName, node, []reflect.Type{
		reflect.TypeFor[*visit.MarkerConstructorDefinition](),
		reflect.TypeFor[*visit.VariantConstructorDefinition](),
	})

	if !ok {
		return
	}

	switch definition := definition.(type) {
	case *visit.MarkerConstructorDefinition:
		// No need to add a condition; markers only have one value

		for _, element := range node.Elements {
			visitor.VisitMatching(element)
			database.SetFact(element, ExtraElementFact{})
		}

		visitor.Constraint(typecheck.InstantiateConstraint(typecheck.Instantiation{
			Source:        node,
			Definition:    definition.Node,
			Substitutions: &map[database.Node]typecheck.Type{},
			Replacements:  map[database.Node]database.Node{definition.Node: node},
		}, visit.GetDefinitionConstraints))

		node.matchingMarkerConstructor = definition
	case *visit.VariantConstructorDefinition:
		matchingVariantConstructorElements := make([]matchingVariantConstructorElement, 0, len(node.Elements))
		for _, element := range node.Elements {
			temporary := visitor.VisitMatching(element)
			matchingVariantConstructorElements = append(matchingVariantConstructorElements, matchingVariantConstructorElement{
				temporary: temporary,
				pattern:   element,
			})
		}

		node.matchingVariantConstructor = &matchingVariantConstructor{
			index:    definition.Index,
			elements: matchingVariantConstructorElements,
		}

		if len(node.Elements) == 0 {
			visitor.Constraint(typecheck.InstantiateConstraint(typecheck.Instantiation{
				Source:        node,
				Definition:    definition.Node,
				Substitutions: &map[database.Node]typecheck.Type{},
				Replacements:  map[database.Node]database.Node{definition.Node: node},
			}, visit.GetDefinitionConstraints))
		} else {
			constructorNode := &database.HiddenNode{
				Facts: database.NewFacts(database.GetSpanFact(node)),
			}
			visitor.Db.Register(constructorNode)
			database.SetFact(constructorNode, typecheck.TypedFact{})

			visitor.Constraint(typecheck.InstantiateConstraint(typecheck.Instantiation{
				Source:        node,
				Definition:    definition.Node,
				Substitutions: &map[database.Node]typecheck.Type{},
				Replacements:  map[database.Node]database.Node{definition.Node: constructorNode},
			}, visit.GetDefinitionConstraints))

			visitor.Constraint(typecheck.TypeConstraint(constructorNode, typecheck.FunctionType[database.Node](node.Elements, node)))
		}
	}
}

func (node *ConstructorPatternNode) Codegen(c *codegen.Codegen) error {
	span := database.GetSpanFact(node)

	switch {
	case node.matchingMarkerConstructor != nil:
		// No code needed
		return nil
	case node.matchingVariantConstructor != nil:
		c.WriteString(span, " && (")
		c.WriteNode(span, node.matching)
		c.WriteString(span, "[runtime.variant] === ")
		c.WriteString(span, fmt.Sprintf("%d", node.matchingVariantConstructor.index))
		c.WriteString(span, ")")

		for index, element := range node.matchingVariantConstructor.elements {
			c.WriteString(span, " && ((")
			c.WriteNode(span, element.temporary)
			c.WriteString(span, " = ")
			c.WriteNode(span, node.matching)
			c.WriteString(span, "[")
			c.WriteString(span, fmt.Sprintf("%d", index))
			c.WriteString(span, "]) || true)")

			if err := c.Write(element.pattern); err != nil {
				return err
			}
		}

		return nil
	default:
		return c.Error(node)
	}
}

func (node *ConstructorPatternNode) EachTemporary(f func(database.Node)) {
	if node.matchingVariantConstructor != nil {
		for _, element := range node.matchingVariantConstructor.elements {
			f(element.temporary)
			EachTemporary(element.pattern, f)
		}
	}
}
