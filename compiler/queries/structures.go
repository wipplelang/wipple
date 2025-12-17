package queries

import (
	"wipple/database"
	"wipple/nodes/expressions"
)

func MissingField(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(field string)) {
	if field, ok := database.GetFact[expressions.MissingFieldFact](node); ok {
		f(string(field))
	}
}

func ExtraField(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(field string)) {
	if field, ok := database.GetFact[expressions.ExtraFieldFact](node); ok {
		f(string(field))
	}
}

func DuplicateField(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(struct{})) {
	if _, ok := database.GetFact[expressions.DuplicateFieldFact](node); ok {
		f(struct{}{})
	}
}
