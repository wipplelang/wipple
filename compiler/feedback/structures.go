package feedback

import (
	"wipple/database"
	"wipple/queries"
)

func registerStructures() {
	register(Feedback[string]{
		Id:    "missing-field",
		Rank:  RankSyntax,
		Query: queries.MissingField,
		Render: func(render *Render, node database.Node, field string) {
			render.WriteNode(node)
			render.WriteString(" is missing a pattern for the field ")
			render.WriteCode(field)
			render.WriteString(".")
			render.WriteBreak()
			render.WriteString("Try adding a pattern for this field using ")
			render.WriteCode(":")
			render.WriteString(".")
		},
	})

	register(Feedback[string]{
		Id:    "extra-field",
		Rank:  RankSyntax,
		Query: queries.ExtraField,
		Render: func(render *Render, node database.Node, field string) {
			render.WriteCode(field)
			render.WriteString(" isn't a field on this structure.")
			render.WriteBreak()
			render.WriteString("Double-check your spelling.")
		},
	})

	register(Feedback[struct{}]{
		Id:    "duplicate-field",
		Rank:  RankSyntax,
		Query: queries.DuplicateField,
		Render: func(render *Render, node database.Node, data struct{}) {
			render.WriteNode(node)
			render.WriteString(" is defined more than once.")
			render.WriteBreak()
			render.WriteString("Try removing this field.")
		},
	})
}
