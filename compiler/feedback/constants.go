package feedback

import (
	"wipple/database"
	"wipple/queries"
)

func registerConstants() {
	register(Feedback[struct{}]{
		Id:    "missing-constant-value",
		Rank:  RankSyntax,
		Query: queries.MissingConstantValue,
		Render: func(render *Render, node database.Node, data struct{}) {
			render.WriteNode(node)
			render.WriteString(" is missing a value.")
			render.WriteBreak()
			render.WriteString("Try giving defining a value for this constant using ")
			render.WriteCode(":")
			render.WriteString(" on the following line.")
		},
	})
}
