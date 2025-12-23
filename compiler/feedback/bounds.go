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
		group    *typecheck.Group
		comments queries.CommentsData
		trace    []typecheck.Constraint
	}

	register(Feedback[errorInstanceData]{
		Id:   "error-instance",
		Rank: RankCustom,
		Query: func(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(data errorInstanceData)) {
			typed, ok := database.GetFact[typecheck.TypedFact](node)
			if !ok || typed.Group == nil {
				return
			}

			queries.ErrorInstance(db, node, filter, func(bound typecheck.ResolvedBound, comments queries.CommentsData, trace []typecheck.Constraint) {
				f(errorInstanceData{
					bound:    bound,
					group:    typed.Group,
					comments: comments,
					trace:    trace,
				})
			})
		},
		On: func(data errorInstanceData) []database.Node {
			var nodes []database.Node

			// If the error instance links to a single node, put that node
			// first...
			if len(data.comments.Links) == 1 {
				for _, link := range data.comments.Links {
					nodes = append(nodes, link.Node)
				}
			} else {
				// ...otherwise, put the source first
				nodes = append(nodes, data.bound.Source)

				for _, link := range data.comments.Links {
					nodes = append(nodes, link.Node)
				}
			}

			nodes = append(nodes, data.comments.Nodes...)
			nodes = append(nodes, data.group.Nodes...)

			database.SortByProximity(nodes, data.bound.Source)

			return nodes
		},
		Render: func(render *Render, node database.Node, data errorInstanceData) {
			render.WriteComments(data.comments)

			if len(data.trace) > 0 {
				render.WriteBreak()
				seen := map[database.Node]struct{}{}
				for _, constraint := range data.trace {
					render.WriteConstraint("\n\n  -  ", constraint, seen)
				}
			}
		},
	})
}
