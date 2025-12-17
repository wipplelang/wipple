package queries

import (
	"wipple/database"
	"wipple/typecheck"
)

func UnresolvedBound(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(bound typecheck.ResolvedBound)) {
	bounds, ok := database.GetFact[typecheck.BoundsFact](node)
	if !ok {
		return
	}

	for _, bound := range bounds {
		if bound.Instance == nil {
			f(bound.Bound)
		}
	}
}
