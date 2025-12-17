package attributes

import (
	"wipple/database"
	"wipple/syntax"
)

type ExtraAttributeValueFact struct{}

func (fact ExtraAttributeValueFact) String() string {
	return "extra attribute value"
}

type DuplicateAttributeFact struct{}

func (fact DuplicateAttributeFact) String() string {
	return "duplicate attribute"
}

type MismatchedAttributeValueFact struct{}

func (fact MismatchedAttributeValueFact) String() string {
	return "mismatched attribute value"
}

type MissingAttributeValueFact struct{}

func (fact MissingAttributeValueFact) String() string {
	return "missing attribute value"
}

func ParseAttributes(parser *syntax.Parser) ([]*AttributeNode, *syntax.Error) {
	return syntax.ParseLines(parser, 0, false, ParseAttribute)
}

type AttributeNode struct {
	Name  string
	Value database.Node
	Facts *database.Facts
}

func (node *AttributeNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseAttribute(parser *syntax.Parser) (*AttributeNode, *syntax.Error) {
	span := parser.Spanned()

	_, err := parser.Token("LeftBracket")
	if err != nil {
		return nil, err
	}

	name, err := syntax.ParseAttributeName(parser)
	if err != nil {
		return nil, err
	}

	value, _, err := syntax.ParseOptional(parser, func(parser *syntax.Parser) (database.Node,
		*syntax.Error) {
		_, err := parser.Token("AssignOperator")
		if err != nil {
			return nil, err
		}

		parser.ConsumeLineBreaks()

		return ParseAttributeValue(parser)
	})
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("RightBracket")
	if err != nil {
		return nil, err
	}

	return &AttributeNode{
		Name:  name,
		Value: value,
		Facts: database.NewFacts(span()),
	}, nil
}

func HasNameAttribute(attributes []*AttributeNode, name string) bool {
	found := false
	for _, attribute := range attributes {
		if attribute.Name == name {
			if attribute.Value != nil {
				database.SetFact(attribute, ExtraAttributeValueFact{})
			} else if found {
				database.SetFact(attribute, DuplicateAttributeFact{})
			} else {
				found = true
			}
		}
	}

	return found
}

func HasAssignmentAttribute[T any](attributes []*AttributeNode, name string, f func(database.Node) *T) *T {
	var result *T
	for _, attribute := range attributes {
		if attribute.Name == name {
			if attribute.Value != nil {
				if result != nil {
					database.SetFact(attribute, DuplicateAttributeFact{})
					continue
				}

				result = f(attribute.Value)

				if result == nil {
					database.SetFact(attribute, MismatchedAttributeValueFact{})
				}
			} else {
				database.SetFact(attribute, MissingAttributeValueFact{})
			}
		}
	}

	return result
}

func HasStringAttributeValue(attributes []*AttributeNode, name string) *string {
	return HasAssignmentAttribute(attributes, name, func(node database.Node) *string {
		if stringNode, ok := node.(*StringAttributeValueNode); ok {
			return &stringNode.Value
		}

		return nil
	})
}
