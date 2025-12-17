package feedback

import (
	"wipple/database"
	"wipple/queries"
)

func registerPatterns() {
	register(Feedback[struct{}]{
		Id:    "nested-set-pattern",
		Rank:  RankSyntax,
		Query: queries.NestedSetPattern,
		Render: func(render *Render, node database.Node, data struct{}) {
			render.WriteNode(node)
			render.WriteString(" cannot be used inside another pattern.")
			render.WriteBreak()
			render.WriteCode("set")
			render.WriteString(" can only be used immediately before a variable assignment using ")
			render.WriteCode(":")
			render.WriteString(".")
		},
	})

	register(Feedback[struct{}]{
		Id:    "extra-element",
		Rank:  RankSyntax,
		Query: queries.ExtraElement,
		Render: func(render *Render, node database.Node, data struct{}) {
			render.WriteNode(node)
			render.WriteString(" can't be used here because this code matches against a marker type, not a variant.")
			render.WriteBreak()
			render.WriteString("Try removing this element.")
		},
	})
}
