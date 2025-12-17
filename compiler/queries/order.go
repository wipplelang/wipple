package queries

import (
	"wipple/database"
	"wipple/nodes/expressions"
	"wipple/nodes/patterns"
)

func OrderGroupNodes(node database.Node) int {
	// Prefer showing patterns first, then expressions

	if _, ok := database.GetFact[patterns.IsPatternFact](node); ok {
		return 0
	}

	if _, ok := database.GetFact[expressions.IsExpressionFact](node); ok {
		return 1
	}

	return 2
}
