package constraints

import (
	"wipple/database"
	"wipple/nodes/types"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type BoundConstraintNode struct {
	Trait      string
	Parameters []database.Node
	Facts      *database.Facts
}

func (node *BoundConstraintNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseBoundConstraint(parser *syntax.Parser) (*BoundConstraintNode, *syntax.Error) {
	span := parser.Spanned()

	_, err := parser.Token("LeftParenthesis", syntax.TokenConfig{
		Reason: "between these parentheses",
	})
	if err != nil {
		return nil, err
	}

	trait, err := syntax.ParseTypeName(parser)
	if err != nil {
		return nil, err
	}

	many, err := syntax.ParseMany(parser, 0, types.ParseAtomicType, syntax.ParseNothing)
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("RightParenthesis")
	if err != nil {
		return nil, err
	}

	parameters := make([]database.Node, 0, len(many))
	for _, item := range many {
		parameters = append(parameters, item.Value)
	}

	return &BoundConstraintNode{
		Trait:      trait,
		Parameters: parameters,
		Facts:      database.NewFacts(span()),
	}, nil
}

func (node *BoundConstraintNode) Visit(visitor *visit.Visitor) {
	visitConstraint(visitor, node)

	trait, ok := visit.Resolve[*visit.TraitDefinition](visitor, node.Trait, node)
	if !ok {
		return
	}

	for _, parameter := range node.Parameters {
		visitor.Visit(parameter)
	}

	substitutions := make(map[database.Node]typecheck.Type, len(trait.Parameters))
	missing := make([]database.Node, 0, len(trait.Parameters))
	for i, parameter := range trait.Parameters {
		if i < len(node.Parameters) {
			substitutions[parameter] = node.Parameters[i]
		} else {
			missing = append(missing, parameter)
		}
	}

	if len(missing) > 0 {
		database.SetFact(node, types.MissingTypesFact(missing))
	}

	for _, extra := range node.Parameters[len(trait.Parameters):] {
		database.SetFact(extra, types.ExtraTypeFact{})
	}

	visitor.Constraint(typecheck.NewBoundConstraint(node, typecheck.UnresolvedBound{
		Source:        node,
		Trait:         trait.Node,
		Substitutions: &substitutions,
		TraitName:     trait.Name,
	}, visit.GetDefinitionConstraints))
}
