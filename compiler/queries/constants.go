package queries

import (
	"wipple/database"
	"wipple/nodes/statements"
)

func MissingConstantValue(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(struct{})) {
	if _, ok := database.GetFact[statements.MissingConstantValueFact](node); ok {
		f(struct{}{})
	}
}
