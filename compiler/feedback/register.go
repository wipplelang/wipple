package feedback

import (
	"wipple/database"
)

type FeedbackItem struct {
	Id     string
	Rank   Rank
	On     []database.Node
	String func() string
}

type Feedback[T any] struct {
	Id     string
	Rank   Rank
	Query  func(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(data T))
	On     func(data T) []database.Node // defaults to the queried node
	Render func(render *Render, node database.Node, data T)
}

var registered = []func(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(item FeedbackItem)){}

func register[T any](entry Feedback[T]) {
	registered = append(registered, func(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(item FeedbackItem)) {
		entry.Query(db, node, filter, func(data T) {
			var on []database.Node
			if entry.On != nil {
				on = entry.On(data)
			}
			if len(on) == 0 {
				on = []database.Node{node}
			}

			f(FeedbackItem{
				Id:   entry.Id,
				Rank: entry.Rank,
				On:   on,
				String: func() string {
					render := NewRender(db)
					entry.Render(render, on[0], data)
					return render.Finish()
				},
			})
		})
	})
}

func init() {
	registerAttributes()
	registerBounds()
	registerConstants()
	registerFormat()
	registerInstances()
	registerNames()
	registerPatterns()
	registerPlaceholders()
	registerStructures()
	registerSyntax()
	registerTypeDefinitions()
	registerTypes()
}

func Collect(db *database.Db, nodeFilter func(node database.Node) bool, itemFilter func(item FeedbackItem) bool) []FeedbackItem {
	var items []FeedbackItem
	database.ContainsNode(db, func(node database.Node) (struct{}, bool) {
		if nodeFilter(node) {
			for _, run := range registered {
				run(db, node, nodeFilter, func(item FeedbackItem) {
					if itemFilter(item) {
						items = append(items, item)
					}
				})
			}
		}

		return struct{}{}, false
	})

	return sort(items)
}
