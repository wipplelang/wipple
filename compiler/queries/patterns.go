package queries

import (
	"wipple/database"
	"wipple/nodes/patterns"
)

func NestedSetPattern(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(struct{})) {
	if _, ok := database.GetFact[patterns.InvalidSetPatternFact](node); ok {
		f(struct{}{})
	}
}

func ExtraElement(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(struct{})) {
	if _, ok := database.GetFact[patterns.ExtraElementFact](node); ok {
		f(struct{}{})
	}
}
