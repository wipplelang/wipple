package visit

import (
	"wipple/database"
	"wipple/nodes/attributes"
)

type DefinedFact struct {
	Definition Definition
}

func (fact DefinedFact) String() string {
	return "is a definition"
}

type Definition interface {
	GetName() string
	GetNode() database.Node
	GetComments() []string
}

type VariableDefinition struct {
	Name  string
	Node  database.Node
	Value database.Node
}

func (definition *VariableDefinition) GetName() string {
	return definition.Name
}

func (definition *VariableDefinition) GetNode() database.Node {
	return definition.Node
}

func (definition *VariableDefinition) GetComments() []string {
	return nil
}

type ConstantDefinition struct {
	Name       string
	Node       database.Node
	Comments   []string
	Attributes ConstantAttributes
	Assigned   bool
	Value      database.Node
}

func (definition *ConstantDefinition) GetName() string {
	return definition.Name
}

func (definition *ConstantDefinition) GetNode() database.Node {
	return definition.Node
}

func (definition *ConstantDefinition) GetComments() []string {
	return definition.Comments
}

type ConstantAttributes struct {
	Unit bool
}

func ParseConstantAttributes(attrs []*attributes.AttributeNode) ConstantAttributes {
	return ConstantAttributes{
		Unit: attributes.HasNameAttribute(attrs, "unit"),
	}
}

type TypeDefinition struct {
	Name       string
	Node       database.Node
	Comments   []string
	Attributes TypeAttributes
	Parameters []database.Node
}

func (definition *TypeDefinition) GetName() string {
	return definition.Name
}

func (definition *TypeDefinition) GetNode() database.Node {
	return definition.Node
}

func (definition *TypeDefinition) GetComments() []string {
	return definition.Comments
}

type TypeAttributes struct {
	Intrinsic bool
}

func ParseTypeAttributes(attrs []*attributes.AttributeNode) TypeAttributes {
	return TypeAttributes{
		Intrinsic: attributes.HasNameAttribute(attrs, "intrinsic"),
	}
}

type TraitDefinition struct {
	Name       string
	Node       database.Node
	Comments   []string
	Attributes TraitAttributes
	Parameters []database.Node
}

func (definition *TraitDefinition) GetName() string {
	return definition.Name
}

func (definition *TraitDefinition) GetNode() database.Node {
	return definition.Node
}

func (definition *TraitDefinition) GetComments() []string {
	return definition.Comments
}

type TraitAttributes struct{}

func ParseTraitAttributes(attrs []*attributes.AttributeNode) TraitAttributes {
	return TraitAttributes{}
}

type InstanceDefinition struct {
	Node       database.Node
	Comments   []string
	Attributes InstanceAttributes
	Value      database.Node
	Trait      database.Node
}

func (definition *InstanceDefinition) GetName() string {
	return "instance"
}

func (definition *InstanceDefinition) GetNode() database.Node {
	return definition.Node
}

func (definition *InstanceDefinition) GetComments() []string {
	return definition.Comments
}

type InstanceAttributes struct {
	Default bool
	Error   bool
}

func ParseInstanceAttributes(attrs []*attributes.AttributeNode) InstanceAttributes {
	return InstanceAttributes{
		Default: attributes.HasNameAttribute(attrs, "default"),
		Error:   attributes.HasNameAttribute(attrs, "error"),
	}
}

type TypeParameterDefinition struct {
	Name string
	Node database.Node
}

func (definition *TypeParameterDefinition) GetName() string {
	return definition.Name
}

func (definition *TypeParameterDefinition) GetNode() database.Node {
	return definition.Node
}

func (definition *TypeParameterDefinition) GetComments() []string {
	return nil
}

type MarkerConstructorDefinition struct {
	Name     string
	Node     database.Node
	Comments []string
}

func (definition *MarkerConstructorDefinition) GetName() string {
	return definition.Name
}

func (definition *MarkerConstructorDefinition) GetNode() database.Node {
	return definition.Node
}

func (definition *MarkerConstructorDefinition) GetComments() []string {
	return definition.Comments
}

type StructureConstructorDefinition struct {
	Name     string
	Node     database.Node
	Comments []string
	Fields   map[string]database.Node
}

func (definition *StructureConstructorDefinition) GetName() string {
	return definition.Name
}

func (definition *StructureConstructorDefinition) GetNode() database.Node {
	return definition.Node
}

func (definition *StructureConstructorDefinition) GetComments() []string {
	return definition.Comments
}

type VariantConstructorDefinition struct {
	Name  string
	Node  database.Node
	Index int
}

func (definition *VariantConstructorDefinition) GetName() string {
	return definition.Name
}

func (definition *VariantConstructorDefinition) GetNode() database.Node {
	return definition.Node
}

func (definition *VariantConstructorDefinition) GetComments() []string {
	return nil
}
