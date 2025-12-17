package feedback

import (
	"wipple/database"
	"wipple/queries"
)

func registerInstances() {
	register(Feedback[struct{}]{
		Id:    "missing-instance-value",
		Rank:  RankSyntax,
		Query: queries.MissingInstanceValue,
		Render: func(render *Render, node database.Node, data struct{}) {
			render.WriteNode(node)
			render.WriteString(" is missing a value.")
			render.WriteBreak()
			render.WriteString("Try adding a value for this instance using ")
			render.WriteCode(":")
			render.WriteString(".")
		},
	})

	register(Feedback[struct{}]{
		Id:    "extra-instance-value",
		Rank:  RankSyntax,
		Query: queries.ExtraInstanceValue,
		Render: func(render *Render, node database.Node, data struct{}) {
			render.WriteString("This instance doesn't need a value because it is marked with ")
			render.WriteCode("[error]")
			render.WriteString(".")
			render.WriteBreak()
			render.WriteString("Remove this code.")
		},
	})
}
