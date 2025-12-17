package queries

import (
	"wipple/database"
	"wipple/nodes/statements"
)

func MissingInstanceValue(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(struct{})) {
	if _, ok := database.GetFact[statements.MissingInstanceValueFact](node); ok {
		f(struct{}{})
	}
}

func ExtraInstanceValue(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(struct{})) {
	if _, ok := database.GetFact[statements.ExtraInstanceValueFact](node); ok {
		f(struct{}{})
	}
}
