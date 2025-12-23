package queries

import (
	"wipple/database"
	"wipple/nodes/expressions"
	"wipple/nodes/patterns"
	"wipple/nodes/types"
	"wipple/typecheck"
)

func OrderGroupNodes(node database.Node) int {
	// Use the original node if instantiated
	if instantiated, ok := database.GetFact[typecheck.InstantiatedFact](node); ok {
		return OrderGroupNodes(instantiated.From)
	}

	// Prefer showing patterns first, then expressions, then types

	if _, ok := database.GetFact[patterns.IsPatternFact](node); ok {
		return 0
	}

	if _, ok := database.GetFact[expressions.IsExpressionFact](node); ok {
		return 1
	}

	if _, ok := database.GetFact[types.IsTypeFact](node); ok {
		return 2
	}

	return 3
}
