package queries

import (
	"wipple/database"
	"wipple/nodes/attributes"
)

func ExtraAttributeValue(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(struct{})) {
	if _, ok := database.GetFact[attributes.ExtraAttributeValueFact](node); ok {
		f(struct{}{})
	}
}

func DuplicateAttribute(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(struct{})) {
	if _, ok := database.GetFact[attributes.DuplicateAttributeFact](node); ok {
		f(struct{}{})
	}
}

func MismatchedAttributeValue(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(struct{})) {
	if _, ok := database.GetFact[attributes.MismatchedAttributeValueFact](node); ok {
		f(struct{}{})
	}
}

func MissingAttributeValue(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(struct{})) {
	if _, ok := database.GetFact[attributes.MissingAttributeValueFact](node); ok {
		f(struct{}{})
	}
}
