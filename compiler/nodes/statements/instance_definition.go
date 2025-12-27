package statements

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/nodes/attributes"
	"wipple/nodes/constraints"
	"wipple/nodes/types"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type MissingInstanceValueFact struct{}

func (fact MissingInstanceValueFact) String() string {
	return "is missing instance value"
}

type ExtraInstanceValueFact struct{}

func (fact ExtraInstanceValueFact) String() string {
	return "is extra instance value"
}

type InstanceDefinitionNode struct {
	Comments    []string
	Attributes  []*attributes.AttributeNode
	Bound       *constraints.BoundConstraintNode
	Constraints []database.Node
	Value       database.Node
	Facts       *database.Facts
}

func (node *InstanceDefinitionNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseInstanceDefinitionStatement(parser *syntax.Parser, parseExpression ParseExpressionFunc) (*InstanceDefinitionNode, *syntax.Error) {
	span := parser.Spanned()

	comments, err := ParseComments(parser)
	if err != nil {
		return nil, err
	}

	attributes, err := attributes.ParseAttributes(parser)
	if err != nil {
		return nil, err
	}

	bound, constraints, err := ParseInstanceConstraints(parser)
	if err != nil {
		return nil, err
	}

	value, _, err := syntax.ParseOptional(parser, func(parser *syntax.Parser) (database.Node, *syntax.Error) {
		_, err := parser.Token("AssignOperator")
		if err != nil {
			return nil, err
		}

		parser.Commit("in this instance definition")

		parser.ConsumeLineBreaks()

		return parseExpression(parser)
	})
	if err != nil {
		return nil, err
	}

	return &InstanceDefinitionNode{
		Comments:    comments,
		Attributes:  attributes,
		Bound:       bound,
		Constraints: constraints,
		Value:       value,
		Facts:       database.NewFacts(span()),
	}, nil
}

func ParseInstanceConstraints(parser *syntax.Parser) (*constraints.BoundConstraintNode, []database.Node, *syntax.Error) {
	_, err := parser.Token("InstanceKeyword")
	if err != nil {
		return nil, nil, err
	}

	parser.Commit("in this instance definition")

	bound, err := constraints.ParseBoundConstraint(parser)
	if err != nil {
		return nil, nil, err
	}

	constraints, _, err := syntax.ParseOptional(parser, constraints.ParseConstraints)
	if err != nil {
		return nil, nil, err
	}

	return bound, constraints, nil
}

func (node *InstanceDefinitionNode) Visit(visitor *visit.Visitor) {
	visit.Defining(visitor, node, func() (*visit.InstanceDefinition, bool) {
		visitor.PushScope()

		attributes := visit.ParseInstanceAttributes(node.Attributes)

		trait := new(database.Node)
		substitutions := new(map[database.Node]typecheck.Type)
		visitor.AfterTypeDefinitions(func() {
			traitDefinition, ok := visit.Resolve[*visit.TraitDefinition](visitor, node.Bound.Trait, node)
			if !ok {
				return
			}

			*trait = traitDefinition.Node

			visitor.CurrentDefinition.WithImplicitTypeParameters(func() {
				for _, parameter := range node.Bound.Parameters {
					visitor.Visit(parameter)
					visitor.Db.Graph.Edge(parameter, node, "parameter")
				}

				*substitutions = make(map[database.Node]typecheck.Type, len(traitDefinition.Parameters))
				missing := make([]database.Node, 0, len(traitDefinition.Parameters))
				for i, parameter := range traitDefinition.Parameters {
					if i < len(node.Bound.Parameters) {
						(*substitutions)[parameter] = node.Bound.Parameters[i]
					} else {
						missing = append(missing, parameter)
					}
				}

				if len(missing) > 0 {
					database.SetFact(node, types.MissingTypesFact(missing))
				}

				if len(node.Bound.Parameters) > len(traitDefinition.Parameters) {
					for _, extra := range node.Bound.Parameters[len(traitDefinition.Parameters):] {
						database.SetFact(extra, types.ExtraTypeFact{})
					}
				}

				for _, constraint := range node.Constraints {
					visitor.Visit(constraint)
					visitor.Db.Graph.Edge(constraint, node, "constraint")
				}

				visitor.Constraint(typecheck.NewInstantiateConstraint(typecheck.Instantiation{
					Source:        node,
					Definition:    traitDefinition.Node,
					Substitutions: substitutions,
					Replacements:  map[database.Node]database.Node{traitDefinition.Node: node},
				}, visit.GetDefinitionConstraints))
			})
		})

		definition := &visit.InstanceDefinition{
			Node:       node,
			Comments:   node.Comments,
			Attributes: attributes,
			Value:      node.Value,
		}

		visitor.AfterAllDefinitions(func() {
			if *trait == nil || *substitutions == nil {
				return
			}

			if node.Value != nil {
				visitor.CurrentDefinition.WithinConstantValue = true

				visitor.Visit(node.Value)
				visitor.Db.Graph.Edge(node.Value, node, "value")
				visitor.Constraint(typecheck.NewGroupConstraint(node.Value, node))

				if attributes.Error {
					database.SetFact(node, ExtraInstanceValueFact{})
				}
			} else if !attributes.Error {
				database.SetFact(node, MissingInstanceValueFact{})
			}

			definition.Trait = *trait

			instances, _ := database.GetFact[typecheck.InstancesFact](*trait)
			instances = append(instances, typecheck.Instance{
				Node:          node,
				Trait:         *trait,
				Substitutions: substitutions,
				Default:       attributes.Default,
				Error:         attributes.Error,
			})
			database.SetFact(*trait, instances)
		})

		visitor.PopScope()

		return definition, true
	})
}

func (node *InstanceDefinitionNode) Codegen(c *codegen.Codegen) error {
	// Handled specially in `codegen.Codegen`
	return nil
}
