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

type TraitDefinitionNode struct {
	Comments    []string
	Attributes  []*attributes.AttributeNode
	Name        string
	Parameters  []database.Node
	Type        database.Node
	Constraints []database.Node
	Facts       *database.Facts
}

func (node *TraitDefinitionNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseTraitDefinitionStatement(parser *syntax.Parser) (*TraitDefinitionNode, *syntax.Error) {
	span := parser.Spanned()

	comments, err := ParseComments(parser)
	if err != nil {
		return nil, err
	}

	attributes, err := attributes.ParseAttributes(parser)
	if err != nil {
		return nil, err
	}

	name, err := syntax.ParseTypeName(parser)
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("AssignOperator")
	if err != nil {
		return nil, err
	}

	parser.ConsumeLineBreaks()

	parameters, err := types.ParseTypeParameters(parser)
	if err != nil {
		return nil, err
	}

	ty, constraints, err := ParseTraitConstraints(parser)
	if err != nil {
		return nil, err
	}

	return &TraitDefinitionNode{
		Comments:    comments,
		Attributes:  attributes,
		Name:        name,
		Parameters:  typeParameterNodes(parameters),
		Type:        ty,
		Constraints: constraints,
		Facts:       database.NewFacts(span()),
	}, nil
}

func ParseTraitConstraints(parser *syntax.Parser) (database.Node, []database.Node, *syntax.Error) {
	_, err := parser.Token("TraitKeyword")
	if err != nil {
		return nil, nil, err
	}

	parser.Commit("in this trait definition")

	traitType, err := types.ParseAtomicType(parser)
	if err != nil {
		return nil, nil, err
	}

	constraints, _, err := syntax.ParseOptional(parser, constraints.ParseConstraints)
	if err != nil {
		return nil, nil, err
	}

	return traitType, constraints, nil
}

func (node *TraitDefinitionNode) Visit(visitor *visit.Visitor) {
	visit.Defining(visitor, node, func() (*visit.TraitDefinition, bool) {
		visitor.PushScope()

		visitor.CurrentDefinition.WithImplicitTypeParameters(func() {
			for _, parameter := range node.Parameters {
				visitor.Visit(parameter)
			}
		})

		visitor.AfterAllDefinitions(func() {
			visitor.Visit(node.Type)

			visitor.Constraint(typecheck.NewGroupConstraint(node, node.Type))

			for _, constraint := range node.Constraints {
				visitor.Visit(constraint)
			}

			// The bound for this trait is added where needed
		})

		visitor.PopScope()

		definition := &visit.TraitDefinition{
			Name:       node.Name,
			Node:       node,
			Comments:   node.Comments,
			Attributes: visit.ParseTraitAttributes(node.Attributes),
			Parameters: node.Parameters,
		}

		visitor.Define(node.Name, definition)

		return definition, true
	})
}

func (node *TraitDefinitionNode) Codegen(c *codegen.Codegen) error {
	// Handled specially in `codegen.Codegen`
	return nil
}
