package queries

import (
	"wipple/database"
	"wipple/visit"
)

func Unresolved(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(name string)) {
	fact, ok := database.GetFact[visit.ResolvedFact](node)
	if !ok {
		return
	}

	if len(fact.Definitions) == 0 {
		f(fact.Name)
	}
}

func Ambiguous(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(name string)) {
	fact, ok := database.GetFact[visit.ResolvedFact](node)
	if !ok {
		return
	}

	if len(fact.Definitions) > 1 {
		f(fact.Name)
	}
}

type DocumentationData struct {
	Declaration string
	Comments    CommentsData
}

func Documentation(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(name string, data DocumentationData)) {
	fact, ok := database.GetFact[visit.DefinedFact](node)
	if !ok {
		return
	}

	var commentsData CommentsData
	Comments(db, fact.Definition.GetNode(), filter, func(c CommentsData) {
		commentsData = c
	})

	data := DocumentationData{
		Declaration: database.DefinitionSource(fact.Definition.GetNode()),
		Comments:    commentsData,
	}

	f(fact.Definition.GetName(), data)
}
