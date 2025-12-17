package feedback

import (
	"wipple/database"
	"wipple/queries"
)

func registerTypeDefinitions() {
	register(Feedback[struct{}]{
		Id:    "duplicate-field-definition",
		Rank:  RankSyntax,
		Query: queries.DuplicateFieldDefinition,
		Render: func(render *Render, node database.Node, data struct{}) {
			render.WriteNode(node)
			render.WriteString(" is already defined in this type.")
			render.WriteBreak()
			render.WriteString("Try renaming this field.")
		},
	})

	register(Feedback[struct{}]{
		Id:    "duplicate-variant-definition",
		Rank:  RankSyntax,
		Query: queries.DuplicateVariantDefinition,
		Render: func(render *Render, node database.Node, data struct{}) {
			render.WriteNode(node)
			render.WriteString(" is already defined in this type.")
			render.WriteBreak()
			render.WriteString("Try renaming this variant.")
		},
	})
}
