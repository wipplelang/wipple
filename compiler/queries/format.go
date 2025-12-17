package queries

import (
	"wipple/database"
	"wipple/nodes/expressions"
)

func MissingFormatInputs(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(count int)) {
	if count, ok := database.GetFact[expressions.MissingFormatInputsFact](node); ok && count > 0 {
		f(int(count))
	}
}

func ExtraFormatInput(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(struct{})) {
	if _, ok := database.GetFact[expressions.ExtraFormatInputFact](node); ok {
		f(struct{}{})
	}
}
