package feedback

import (
	"wipple/database"
	"wipple/queries"
)

func registerAttributes() {
	register(Feedback[struct{}]{
		Id:    "extra-attribute-value",
		Rank:  RankSyntax,
		Query: queries.ExtraAttributeValue,
		Render: func(render *Render, node database.Node, data struct{}) {
			render.WriteNode(node)
			render.WriteString(" doesn't accept a value.")
			render.WriteBreak()
			render.WriteString("Try removing the value from this attribute.")
		},
	})

	register(Feedback[struct{}]{
		Id:    "duplicate-attribute",
		Rank:  RankSyntax,
		Query: queries.DuplicateAttribute,
		Render: func(render *Render, node database.Node, data struct{}) {
			render.WriteNode(node)
			render.WriteString(" is defined more than once.")
			render.WriteBreak()
			render.WriteString("Try removing this attribute.")
		},
	})

	register(Feedback[struct{}]{
		Id:    "missing-attribute-value",
		Rank:  RankSyntax,
		Query: queries.MissingAttributeValue,
		Render: func(render *Render, node database.Node, data struct{}) {
			render.WriteNode(node)
			render.WriteString(" is missing a value.")
			render.WriteBreak()
			render.WriteString("Try adding a value to this attribute using ")
			render.WriteCode(":")
			render.WriteString(".")
		},
	})
}
