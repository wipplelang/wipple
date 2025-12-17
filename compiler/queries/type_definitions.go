package queries

import (
	"wipple/database"
	"wipple/nodes/statements"
)

func DuplicateFieldDefinition(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(struct{})) {
	if _, ok := database.GetFact[statements.DuplicateFieldDefinitionFact](node); ok {
		f(struct{}{})
	}
}

func DuplicateVariantDefinition(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(struct{})) {
	if _, ok := database.GetFact[statements.DuplicateVariantDefinitionFact](node); ok {
		f(struct{}{})
	}
}
