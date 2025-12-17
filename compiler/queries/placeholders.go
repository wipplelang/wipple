package queries

import (
	"wipple/database"
	"wipple/nodes/expressions"
	"wipple/typecheck"
)

func Placeholder(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(typecheck.Type)) {
	if _, ok := database.GetFact[expressions.IsPlaceholder](node); ok {
		if fact, ok := database.GetFact[typecheck.TypedFact](node); ok && fact.Group != nil && len(fact.Group.Types) > 0 {
			f(fact.Group.Types[0])
		} else {
			f(nil)
		}
	}
}
