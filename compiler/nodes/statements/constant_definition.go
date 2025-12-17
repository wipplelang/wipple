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

type MissingConstantValueFact struct{}

func (fact MissingConstantValueFact) String() string {
	return "is missing constant value"
}

type ConstantDefinitionNode struct {
	Comments    []string
	Attributes  []*attributes.AttributeNode
	Name        string
	Type        database.Node
	Constraints []database.Node
	Facts       *database.Facts
}

func (node *ConstantDefinitionNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseConstantDefinitionStatement(parser *syntax.Parser) (*ConstantDefinitionNode, *syntax.Error) {
	span := parser.Spanned()

	comments, err := ParseComments(parser)
	if err != nil {
		return nil, err
	}

	attributes, err := attributes.ParseAttributes(parser)
	if err != nil {
		return nil, err
	}

	name, err := syntax.ParseVariableName(parser)
	if err != nil {
		return nil, err
	}

	ty, constraints, err := ParseConstantConstraints(parser)
	if err != nil {
		return nil, err
	}

	return &ConstantDefinitionNode{
		Comments:    comments,
		Attributes:  attributes,
		Name:        name,
		Type:        ty,
		Constraints: constraints,
		Facts:       database.NewFacts(span()),
	}, nil
}

func ParseConstantConstraints(parser *syntax.Parser) (database.Node, []database.Node, *syntax.Error) {
	_, err := parser.Token("AnnotateOperator", syntax.TokenConfig{
		Commit: "in this constant definition",
	})
	if err != nil {
		return nil, nil, err
	}

	parser.ConsumeLineBreaks()

	constantType, err := types.ParseType(parser)
	if err != nil {
		return nil, nil, err
	}

	constraints, _, err := syntax.ParseOptional(parser, constraints.ParseConstraints)
	if err != nil {
		return nil, nil, err
	}

	return constantType, constraints, nil
}

func (node *ConstantDefinitionNode) Visit(visitor *visit.Visitor) {
	visit.Defining(visitor, node, func() (*visit.ConstantDefinition, bool) {
		visitor.PushScope()

		visitor.AfterTypeDefinitions(func() {
			visitor.CurrentDefinition.WithImplicitTypeParameters(func() {
				visitor.Visit(node.Type)

				for _, constraint := range node.Constraints {
					visitor.Visit(constraint)
				}
			})

			visitor.Constraint(typecheck.GroupConstraint(node, node.Type))
		})

		visitor.PopScope()

		definition := &visit.ConstantDefinition{
			Name:       node.Name,
			Node:       node,
			Comments:   node.Comments,
			Attributes: visit.ParseConstantAttributes(node.Attributes),
			Assigned:   false,
			Value:      node.Type,
		}

		visitor.Define(node.Name, definition)

		visitor.AfterAllExpressions(func() {
			if !definition.Assigned {
				database.SetFact(node, MissingConstantValueFact{})
			}
		})

		return definition, true
	})
}

func (node *ConstantDefinitionNode) Codegen(c *codegen.Codegen) error {
	// Handled specially in `codegen.Codegen`
	return nil
}
