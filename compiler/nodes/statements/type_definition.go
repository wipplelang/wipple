package statements

import (
	"fmt"

	"wipple/codegen"
	"wipple/database"
	"wipple/nodes/attributes"
	"wipple/nodes/types"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type DuplicateFieldDefinitionFact struct{}

func (fact DuplicateFieldDefinitionFact) String() string {
	return "is duplicate field definition"
}

type DuplicateVariantDefinitionFact struct{}

func (fact DuplicateVariantDefinitionFact) String() string {
	return "is duplicate variant definition"
}

type TypeDefinitionNode struct {
	Comments       []string
	Attributes     []*attributes.AttributeNode
	Name           string
	Parameters     []database.Node
	Representation TypeRepresentation
	Facts          *database.Facts
}

func (node *TypeDefinitionNode) GetFacts() *database.Facts {
	return node.Facts
}

type TypeAttributes struct {
	Intrinsic bool
}

type TypeRepresentation interface {
	GetSpan() database.Span
}

type StructureTypeRepresentation struct {
	Fields []FieldDefinition
	Span   database.Span
}

func (representation *StructureTypeRepresentation) GetSpan() database.Span {
	return representation.Span
}

type FieldDefinition struct {
	Name string
	Type database.Node
	Span database.Span
}

type EnumerationTypeRepresentation struct {
	Variants []VariantDefinition
	Span     database.Span
}

func (representation *EnumerationTypeRepresentation) GetSpan() database.Span {
	return representation.Span
}

type VariantDefinition struct {
	Name     string
	Elements []database.Node
	Span     database.Span
}

type MarkerTypeRepresentation struct {
	Span database.Span
}

func (representation *MarkerTypeRepresentation) GetSpan() database.Span {
	return representation.Span
}

func ParseTypeDefinitionStatement(parser *syntax.Parser) (*TypeDefinitionNode, *syntax.Error) {
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

	representation, err := ParseTypeRepresentation(parser)
	if err != nil {
		return nil, err
	}

	return &TypeDefinitionNode{
		Comments:       comments,
		Attributes:     attributes,
		Name:           name,
		Parameters:     typeParameterNodes(parameters),
		Representation: representation,
		Facts:          database.NewFacts(span()),
	}, nil
}

func ParseTypeAttributes(attrs []*attributes.AttributeNode) TypeAttributes {
	return TypeAttributes{
		Intrinsic: attributes.HasNameAttribute(attrs, "intrinsic"),
	}
}

func ParseTypeRepresentation(parser *syntax.Parser) (TypeRepresentation, *syntax.Error) {
	return syntax.ParseCached(parser, func(p *syntax.Parser) (TypeRepresentation, *syntax.Error) {
		structure, ok, err := syntax.ParseOptional(parser, ParseStructureTypeRepresentation)
		if err != nil {
			return nil, err
		}
		if ok {
			return structure, nil
		}

		enumeration, ok, err := syntax.ParseOptional(parser, ParseEnumerationTypeRepresentation)
		if err != nil {
			return nil, err
		}
		if ok {
			return enumeration, nil
		}

		marker, ok, err := syntax.ParseOptional(parser, ParseMarkerTypeRepresentation)
		if err != nil {
			return nil, err
		}
		if ok {
			return marker, nil
		}

		return nil, parser.Error("Expected type representation")
	})
}

func ParseStructureTypeRepresentation(parser *syntax.Parser) (*StructureTypeRepresentation, *syntax.Error) {
	span := parser.Spanned()

	_, err := parser.Token("TypeKeyword")
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("LeftBrace")
	if err != nil {
		return nil, err
	}

	fields, err := syntax.ParseLines(parser, 1, true, ParseFieldDefinition)
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("RightBrace")
	if err != nil {
		return nil, err
	}

	return &StructureTypeRepresentation{
		Fields: fields,
		Span:   span(),
	}, nil
}

func ParseFieldDefinition(parser *syntax.Parser) (FieldDefinition, *syntax.Error) {
	span := parser.Spanned()

	name, err := syntax.ParseVariableName(parser)
	if err != nil {
		return FieldDefinition{}, err
	}

	_, err = parser.Token("AnnotateOperator", syntax.TokenConfig{
		Commit: "in this type annotation",
	})
	if err != nil {
		return FieldDefinition{}, err
	}

	parser.ConsumeLineBreaks()

	fieldType, err := types.ParseType(parser)
	if err != nil {
		return FieldDefinition{}, err
	}

	return FieldDefinition{
		Name: name,
		Type: fieldType,
		Span: span(),
	}, nil
}

func ParseEnumerationTypeRepresentation(parser *syntax.Parser) (*EnumerationTypeRepresentation, *syntax.Error) {
	span := parser.Spanned()

	_, err := parser.Token("TypeKeyword")
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("LeftBrace")
	if err != nil {
		return nil, err
	}

	variants, err := syntax.ParseLines(parser, 1, true, ParseVariantDefinition)
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("RightBrace")
	if err != nil {
		return nil, err
	}

	return &EnumerationTypeRepresentation{
		Variants: variants,
		Span:     span(),
	}, nil
}

func ParseMarkerTypeRepresentation(parser *syntax.Parser) (*MarkerTypeRepresentation, *syntax.Error) {
	span := parser.Spanned()

	_, err := parser.Token("TypeKeyword", syntax.TokenConfig{
		Commit: "in this type definition",
	})
	if err != nil {
		return nil, err
	}

	return &MarkerTypeRepresentation{
		Span: span(),
	}, nil
}

func ParseVariantDefinition(parser *syntax.Parser) (VariantDefinition, *syntax.Error) {
	span := parser.Spanned()

	name, err := syntax.ParseConstructorName(parser)
	if err != nil {
		return VariantDefinition{}, err
	}

	many, ok, err := syntax.ParseOptional(parser, func(parser *syntax.Parser) ([]syntax.Many[database.Node, struct{}], *syntax.Error) {
		return syntax.ParseMany(parser, 1, types.ParseAtomicType, syntax.ParseNothing)
	})
	if err != nil {
		return VariantDefinition{}, err
	}

	var elements []database.Node
	if ok {
		elements = make([]database.Node, 0, len(many))
		for _, item := range many {
			elements = append(elements, item.Value)
		}
	}

	return VariantDefinition{
		Name:     name,
		Elements: elements,
		Span:     span(),
	}, nil
}

func typeParameterNodes(parameters []types.TypeParameterNode) []database.Node {
	nodes := make([]database.Node, len(parameters))
	for i := range parameters {
		nodes[i] = &parameters[i]
	}

	return nodes
}

func (node *TypeDefinitionNode) Visit(visitor *visit.Visitor) {
	visit.Defining(visitor, node, func() (*visit.TypeDefinition, bool) {
		visitor.PushScope()

		visitor.CurrentDefinition.WithImplicitTypeParameters(func() {
			for _, parameter := range node.Parameters {
				visitor.Visit(parameter)
			}
		})

		visitor.Constraint(typecheck.NewTypeConstraint(node, typecheck.NamedType(node, node.Name, node.Parameters)))

		typeConstraints := visit.GetDefinitionConstraints(node)

		// Types don't have additional constraints

		definition := &visit.TypeDefinition{
			Name:       node.Name,
			Node:       node,
			Comments:   node.Comments,
			Attributes: visit.ParseTypeAttributes(node.Attributes),
			Parameters: node.Parameters,
		}

		if !definition.Attributes.Intrinsic {
			visitor.AfterTypeDefinitions(func() {
				switch representation := node.Representation.(type) {
				case *MarkerTypeRepresentation:
					visitor.PopScope()

					visitor.Define(node.Name, &visit.MarkerConstructorDefinition{
						Name:     definition.Name,
						Node:     definition.Node,
						Comments: definition.Comments,
					})
				case *StructureTypeRepresentation:
					fields := make(map[string]database.Node, len(representation.Fields))
					for _, field := range representation.Fields {
						visitor.Visit(field.Type)

						if _, ok := fields[field.Name]; ok {
							database.SetFact(node, DuplicateFieldDefinitionFact{})
							continue
						}

						fields[field.Name] = field.Type
					}

					visitor.PopScope()

					visitor.Define(node.Name, &visit.StructureConstructorDefinition{
						Name:     definition.Name,
						Node:     definition.Node,
						Comments: definition.Comments,
						Fields:   fields,
					})
				case *EnumerationTypeRepresentation:
					variantDefinitions := make(map[string]*visit.VariantConstructorDefinition, len(representation.Variants))
					for i, variant := range representation.Variants {
						if _, ok := variantDefinitions[variant.Name]; ok {
							database.SetFact(node, DuplicateVariantDefinitionFact{})
							continue
						}

						variantNode := &variantNode{
							Facts:   database.NewFacts(variant.Span),
							Index:   i,
							Variant: variant,
						}

						visitor.Db.Register(variantNode)

						database.SetFact(variantNode, typecheck.TypedFact{})

						constructorDefinition, _ := visit.Defining(visitor, variantNode, func() (*visit.VariantConstructorDefinition, bool) {
							// Inherit the type definition's constraints
							for _, constraint := range typeConstraints {
								visitor.Constraint(constraint)
							}

							for _, element := range variant.Elements {
								visitor.Visit(element)
							}

							if len(variant.Elements) == 0 {
								visitor.Constraint(typecheck.NewGroupConstraint(variantNode, node))
							} else {
								visitor.Constraint(typecheck.NewTypeConstraint(variantNode, typecheck.FunctionType[database.Node](variant.Elements, node)))
							}

							constructorDefinition := &visit.VariantConstructorDefinition{
								Name:  variant.Name,
								Node:  variantNode,
								Index: i,
							}

							return constructorDefinition, true
						})

						variantDefinitions[variant.Name] = constructorDefinition
					}

					visitor.PopScope()

					for name, constructorDefinition := range variantDefinitions {
						visitor.Define(name, constructorDefinition)
					}
				}
			})
		}

		visitor.PopScope()

		visitor.Define(node.Name, definition)

		return definition, true
	})
}

func (node *TypeDefinitionNode) Codegen(c *codegen.Codegen) error {
	// Handled specially in `codegen.Codegen`
	return nil
}

type variantNode struct {
	Facts   *database.Facts
	Index   int
	Variant VariantDefinition
}

func (node *variantNode) GetFacts() *database.Facts {
	return node.Facts
}

func (node *variantNode) Codegen(c *codegen.Codegen) error {
	span := database.GetSpanFact(node)

	elementTemporaries := make([]database.Node, len(node.Variant.Elements))
	for i, element := range node.Variant.Elements {
		temporary := &database.HiddenNode{
			Facts: database.NewFacts(database.GetSpanFact(element)),
		}

		c.Db.Register(temporary)

		elementTemporaries[i] = temporary
	}

	if len(elementTemporaries) == 0 {
		c.WriteString(span, fmt.Sprintf("__wipple_variant(%d, [])", node.Index))
	} else {
		c.WriteString(span, "(async (")
		for _, temporary := range elementTemporaries {
			c.WriteNode(span, temporary)
			c.WriteString(span, ", ")
		}
		c.WriteString(span, ") => {")
		c.WriteLine()

		c.WriteString(span, fmt.Sprintf("return __wipple_variant(%d, [", node.Index))
		for _, temporary := range elementTemporaries {
			c.WriteNode(span, temporary)
			c.WriteString(span, ", ")
		}
		c.WriteString(span, "]);")
		c.WriteLine()

		c.WriteString(span, "})")
	}

	return nil
}
