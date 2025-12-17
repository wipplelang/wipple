package queries

import (
	"wipple/database"
	"wipple/syntax"
)

func SyntaxError(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(error syntax.Error)) {
	if err, ok := database.GetFact[syntax.SyntaxErrorFact](node); ok {
		f(syntax.Error(err))
	}
}
