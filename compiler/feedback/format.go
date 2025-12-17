package feedback

import (
	"wipple/database"
	"wipple/queries"
)

func registerFormat() {
	register(Feedback[int]{
		Id:    "missing-format-inputs",
		Rank:  RankSyntax,
		Query: queries.MissingFormatInputs,
		Render: func(render *Render, node database.Node, count int) {
			render.WriteNode(node)
			render.WriteString(" needs ")
			render.WriteNumber(count, "more input", "more inputs")
			render.WriteString(".")
			render.WriteBreak()
			render.WriteString("Try adding code after the string.")
		},
	})

	register(Feedback[struct{}]{
		Id:    "extra-format-input",
		Rank:  RankSyntax,
		Query: queries.ExtraFormatInput,
		Render: func(render *Render, node database.Node, data struct{}) {
			render.WriteNode(node)
			render.WriteString(" isn't used in the format string.")
			render.WriteBreak()
			render.WriteString("Try removing this input or add another ")
			render.WriteCode("_")
			render.WriteString(" placeholder.")
		},
	})
}
