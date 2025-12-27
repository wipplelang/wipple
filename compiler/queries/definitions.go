package queries

import (
	"slices"
	"wipple/database"
	"wipple/visit"
)

func Definitions(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(definitions []database.Node)) {
	var definitions []database.Node
	if resolved, ok := database.GetFact[visit.ResolvedFact](node); ok {
		for _, definition := range resolved.Definitions {
			definitions = append(definitions, definition.GetNode())
		}
	}

	f(definitions)
}

func References(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(references []database.Node)) {
	var references []database.Node
	database.ContainsFact(db, func(other database.Node, fact visit.ResolvedFact) (struct{}, bool) {
		if slices.ContainsFunc(fact.Definitions, func(definition visit.Definition) bool {
			return definition.GetNode() == node
		}) {
			references = append(references, other)
		}

		return struct{}{}, false
	})

	f(references)
}

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
		Declaration: database.DefinitionSource(database.GetSpanFact(fact.Definition.GetNode()).Source),
		Comments:    commentsData,
	}

	f(fact.Definition.GetName(), data)
}
