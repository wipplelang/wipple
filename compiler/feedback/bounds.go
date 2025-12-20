package feedback

import (
	"wipple/database"
	"wipple/queries"
	"wipple/typecheck"
)

func registerBounds() {
	register(Feedback[typecheck.ResolvedBound]{
		Id:    "unresolved-bound",
		Rank:  RankBounds,
		Query: queries.UnresolvedBound,
		Render: func(render *Render, node database.Node, bound typecheck.ResolvedBound) {
			render.WriteNode(node)
			render.WriteString(" requires the instance ")
			render.WriteBound(bound)
			render.WriteString(", but this instance isn't defined.")
			render.WriteBreak()
			render.WriteString("Double-check that these types are correct.")
		},
	})

	type errorInstanceData struct {
		bound    typecheck.ResolvedBound
		comments queries.CommentsData
	}

	register(Feedback[errorInstanceData]{
		Id:   "error-instance",
		Rank: RankCustom,
		Query: func(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(data errorInstanceData)) {
			queries.ErrorInstance(db, node, filter, func(bound typecheck.ResolvedBound, comments queries.CommentsData) {
				f(errorInstanceData{bound: bound, comments: comments})
			})
		},
		On: func(data errorInstanceData) []database.Node {
			if len(data.comments.Links) == 1 {
				for _, link := range data.comments.Links {
					return []database.Node{link.Node}
				}
			}

			return nil
		},
		Render: func(render *Render, node database.Node, data errorInstanceData) {
			render.WriteComments(data.comments)
			render.WriteBreak()
			render.WriteString("(This feedback comes from the instance ")
			render.WriteBound(data.bound)
			render.WriteString(".)")
		},
	})
}
