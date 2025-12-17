package feedback

import (
	"wipple/database"
	"wipple/queries"
	"wipple/syntax"
)

func registerSyntax() {
	register(Feedback[syntax.Error]{
		Id:    "syntax-error",
		Rank:  RankSyntax,
		Query: queries.SyntaxError,
		Render: func(render *Render, node database.Node, err syntax.Error) {
			render.WriteString(err.Message)
			if err.Committed != "" {
				render.WriteString(" ")
				render.WriteString(err.Committed)
			}
			render.WriteString(".")

			if err.Reason != "" {
				render.WriteBreak()
				render.WriteString(err.Reason)
			}

			render.WriteBreak()
			render.WriteString("Check your spelling.")
		},
	})
}
