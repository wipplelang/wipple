package types

import (
	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type TypeParameterNode struct {
	Name  string
	Infer bool
	Value database.Node
	Facts *database.Facts
}

func (node *TypeParameterNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseTypeParameters(parser *syntax.Parser) ([]TypeParameterNode, *syntax.Error) {
	many, err := syntax.ParseMany(parser, 0, ParseTypeParameter, syntax.ParseNothing)
	if err != nil {
		return nil, err
	}

	if len(many) > 0 {
		_, err := parser.Token("TypeFunctionOperator", syntax.TokenConfig{
			Commit: "in this generic item",
		})

		if err != nil {
			return nil, err
		}

		parser.ConsumeLineBreaks()
	}

	parameters := make([]TypeParameterNode, 0, len(many))
	for _, item := range many {
		parameters = append(parameters, *item.Value)
	}

	return parameters, nil
}

func ParseTypeParameter(parser *syntax.Parser) (*TypeParameterNode, *syntax.Error) {
	return syntax.ParseCached(parser, func(p *syntax.Parser) (*TypeParameterNode, *syntax.Error) {
		parameter, ok, err := syntax.ParseOptional(parser, ParseNamedTypeParameter)
		if err != nil {
			return nil, err
		}
		if ok {
			return parameter, nil
		}

		parameter, ok, err = syntax.ParseOptional(parser, ParseInferTypeParameter)
		if err != nil {
			return nil, err
		}
		if ok {
			return parameter, nil
		}

		return nil, parser.Error("Expected type parameter")
	})
}

func ParseAnnotatedParameterType(parser *syntax.Parser) (*TypeParameterNode, *syntax.Error) {
	span := parser.Spanned()

	name, err := syntax.ParseTypeParameterName(parser)
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("AnnotateOperator", syntax.TokenConfig{
		Commit: "in this type annotation",
	})
	if err != nil {
		return nil, err
	}

	parser.ConsumeLineBreaks()

	value, err := ParseType(parser)
	if err != nil {
		return nil, err
	}

	return &TypeParameterNode{
		Name:  name,
		Infer: false,
		Value: value,
		Facts: database.NewFacts(span()),
	}, nil
}

func ParseParameterType(parser *syntax.Parser) (*TypeParameterNode, *syntax.Error) {
	span := parser.Spanned()

	name, err := syntax.ParseTypeParameterName(parser)
	if err != nil {
		return nil, err
	}

	return &TypeParameterNode{
		Name:  name,
		Infer: false,
		Value: nil,
		Facts: database.NewFacts(span()),
	}, nil
}

func ParseNamedTypeParameter(parser *syntax.Parser) (*TypeParameterNode, *syntax.Error) {
	span := parser.Spanned()

	name, err := syntax.ParseTypeParameterName(parser)
	if err != nil {
		return nil, err
	}

	return &TypeParameterNode{
		Name:  name,
		Infer: false,
		Value: nil,
		Facts: database.NewFacts(span()),
	}, nil
}

func ParseInferTypeParameter(parser *syntax.Parser) (*TypeParameterNode, *syntax.Error) {
	span := parser.Spanned()

	_, err := parser.Token("LeftParenthesis", syntax.TokenConfig{
		Reason: "between these parentheses",
	})
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("InferKeyword", syntax.TokenConfig{
		Commit: "in this inferred type parameter",
	})
	if err != nil {
		return nil, err
	}

	name, err := syntax.ParseTypeParameterName(parser)
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("RightParenthesis")
	if err != nil {
		return nil, err
	}

	return &TypeParameterNode{
		Name:  name,
		Infer: true,
		Value: nil,
		Facts: database.NewFacts(span()),
	}, nil
}

func (node *TypeParameterNode) Visit(visitor *visit.Visitor) {
	visitType(visitor, node)

	existing, ok := visit.Resolve[*visit.TypeParameterDefinition](visitor, node.Name, node)
	if ok {
		visitor.Constraint(typecheck.NewGroupConstraint(node, existing.Node))
	} else {
		definition := &visit.TypeParameterDefinition{
			Name: node.Name,
			Node: node,
		}
		visitor.Define(node.Name, definition)

		visitor.Constraint(typecheck.NewTypeConstraint(node, typecheck.ParameterType(node, node.Name)))

		if node.Infer {
			database.SetFact(node, typecheck.InferredParameterFact{})
		}

		// Update the `Resolved` fact
		visit.Resolve[*visit.TypeParameterDefinition](visitor, node.Name, node)

		if node.Value != nil {
			visitor.Visit(node.Value)

			constraint := typecheck.NewGroupConstraint(node, node.Value)
			constraint.Info().IsActive = false // wait until instantiated
			visitor.Constraint(constraint)
		}

		typeParameters, _ := database.GetFact[visit.TypeParametersFact](visitor.CurrentDefinition.Node)
		typeParameters = append(typeParameters, node)
		database.SetFact(visitor.CurrentDefinition.Node, typeParameters)
	}
}
