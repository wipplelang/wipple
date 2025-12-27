package types

import (
	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type NamedTypeNode struct {
	Name       string
	Parameters []database.Node
	Facts      *database.Facts
}

func (node *NamedTypeNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseNamedType(parser *syntax.Parser) (*NamedTypeNode, *syntax.Error) {
	span := parser.Spanned()

	name, err := syntax.ParseTypeName(parser)
	if err != nil {
		return nil, err
	}

	return &NamedTypeNode{
		Name:       name,
		Parameters: nil,
		Facts:      database.NewFacts(span()),
	}, nil
}

func ParseParameterizedType(parser *syntax.Parser) (*NamedTypeNode, *syntax.Error) {
	span := parser.Spanned()

	name, err := syntax.ParseTypeName(parser)
	if err != nil {
		return nil, err
	}

	many, err := syntax.ParseMany(parser, 1, ParseAtomicType, syntax.ParseNothing)
	if err != nil {
		return nil, err
	}

	parameters := make([]database.Node, 0, len(many))
	for _, item := range many {
		parameters = append(parameters, item.Value)
	}

	return &NamedTypeNode{
		Name:       name,
		Parameters: parameters,
		Facts:      database.NewFacts(span()),
	}, nil
}

func (node *NamedTypeNode) Visit(visitor *visit.Visitor) {
	visitType(visitor, node)

	for _, parameter := range node.Parameters {
		visitor.Visit(parameter)
		visitor.Db.Graph.Edge(parameter, node, "parameter")
	}

	typeDefinition, ok := visit.Resolve[*visit.TypeDefinition](visitor, node.Name, node)
	if !ok {
		return
	}

	substitutions := make(map[database.Node]typecheck.Type, len(typeDefinition.Parameters))
	missing := make([]database.Node, 0, len(typeDefinition.Parameters))
	for i, parameter := range typeDefinition.Parameters {
		if i < len(node.Parameters) {
			substitutions[parameter] = node.Parameters[i]
		} else {
			missing = append(missing, parameter)
		}
	}

	if len(missing) > 0 {
		database.SetFact(node, MissingTypesFact(missing))
	}

	if len(node.Parameters) > len(typeDefinition.Parameters) {
		for _, extra := range node.Parameters[len(typeDefinition.Parameters):] {
			database.SetFact(extra, ExtraTypeFact{})
		}
	}

	visitor.Constraint(typecheck.NewInstantiateConstraint(typecheck.Instantiation{
		Source:        node,
		Definition:    typeDefinition.Node,
		Substitutions: &substitutions,
		Replacements:  map[database.Node]database.Node{typeDefinition.Node: node},
	}, visit.GetDefinitionConstraints))
}
