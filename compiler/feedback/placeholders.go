package feedback

import (
	"wipple/database"
	"wipple/queries"
	"wipple/typecheck"
)

func registerPlaceholders() {
	register(Feedback[typecheck.Type]{
		Id:    "placeholder",
		Rank:  RankPlaceholder,
		Query: queries.Placeholder,
		Render: func(render *Render, node database.Node, ty typecheck.Type) {
			if ty != nil {
				render.WriteString("Found a placeholder of type ")
				render.WriteType(ty)
				render.WriteString(".")
			} else {
				render.WriteString("Found a placeholder.")
			}

			render.WriteBreak()

			if ty != nil {
				render.WriteString("Add a ")
				render.WriteType(ty)
				render.WriteString(" value here before running your program.")
			} else {
				render.WriteString("Add a value here before running your program.")
			}
		},
	})
}
