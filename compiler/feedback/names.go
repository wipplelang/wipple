package feedback

import (
	"wipple/database"
	"wipple/queries"
	"wipple/visit"
)

func registerNames() {
	register(Feedback[string]{
		Id:    "unresolved",
		Rank:  RankNames,
		Query: queries.Unresolved,
		Render: func(render *Render, node database.Node, name string) {
			render.WriteString("Can't find ")
			render.WriteCode(name)
			render.WriteString(".")
			render.WriteBreak()
			render.WriteString("Double-check your spelling.")
		},
	})

	type ambiguousData struct {
		Name        string
		Definitions []database.Node
	}

	register(Feedback[ambiguousData]{
		Id:   "ambiguous",
		Rank: RankNames,
		Query: func(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(data ambiguousData)) {
			queries.Ambiguous(db, node, filter, func(name string) {
				var definitions []database.Node
				if fact, ok := database.GetFact[visit.ResolvedFact](node); ok {
					for _, definition := range fact.Definitions {
						definitions = append(definitions, definition.GetNode())
					}
				}

				f(ambiguousData{
					Name:        name,
					Definitions: definitions,
				})
			})
		},
		Render: func(render *Render, node database.Node, data ambiguousData) {
			render.WriteCode(data.Name)
			render.WriteString(" could refer to ")

			items := make([]func(), 0, len(data.Definitions))
			for _, definition := range data.Definitions {
				items = append(items, func() {
					render.WriteNode(definition)
				})
			}
			render.WriteList(items, "or", 3)

			render.WriteString(".")
			render.WriteBreak()
			render.WriteString("Rename the extra definitions.")
		},
	})
}
