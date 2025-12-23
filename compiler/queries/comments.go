package queries

import (
	"slices"
	"wipple/database"
	"wipple/nodes/statements"
	"wipple/nodes/types"
	"wipple/typecheck"
	"wipple/visit"
)

func ErrorInstance(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(bound typecheck.ResolvedBound, comments CommentsData, trace []typecheck.Constraint)) {
	bounds, ok := database.GetFact[typecheck.BoundsFact](node)
	if !ok {
		return
	}

	for _, bound := range bounds {
		if bound.Instance != nil && bound.Error {
			instance, ok := bound.Instance.(*statements.InstanceDefinitionNode)
			if !ok {
				continue
			}

			comments := CommentsData{
				Nodes:    []database.Node{instance},
				Comments: instance.Comments,
				Links:    getLinks(db, instance, node, filter),
			}

			var trace []typecheck.Constraint
			for _, ty := range *bound.Bound.Substitutions {
				if node, ok := ty.(database.Node); ok {
					if fact, ok := database.GetFact[typecheck.TypedFact](node); ok && fact.Group != nil {
						trace = append(trace, fact.Group.Trace...)
						comments.Nodes = append(comments.Nodes, fact.Group.Nodes...)
					}
				}
			}

			slices.SortStableFunc(trace, func(left typecheck.Constraint, right typecheck.Constraint) int {
				return database.CompareSpans(left.Info().Span, right.Info().Span)
			})

			f(bound.Bound, comments, trace)
		}
	}
}

type CommentsData struct {
	Nodes    []database.Node
	Comments []string
	Links    map[string]Link
}

func Comments(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(data CommentsData)) {
	if defined, ok := database.GetFact[visit.DefinedFact](node); ok {
		f(CommentsData{
			Nodes:    []database.Node{node},
			Comments: defined.Definition.GetComments(),
			Links:    getLinks(db, node, node, filter),
		})
	} else if resolved, ok := database.GetFact[visit.ResolvedFact](node); ok && len(resolved.Definitions) == 1 {
		definition := resolved.Definitions[0]

		f(CommentsData{
			Nodes:    []database.Node{definition.GetNode()},
			Comments: definition.GetComments(),
			Links:    getLinks(db, definition.GetNode(), node, filter),
		})
	}

}

type Link struct {
	Node    database.Node
	Related []database.Node
	Types   []*typecheck.ConstructedType
}

func getLinks(db *database.Db, node database.Node, source database.Node, filter func(node database.Node) bool) map[string]Link {
	links := map[string]Link{}

	fact, ok := database.GetFact[visit.TypeParametersFact](node)
	if !ok {
		return links
	}

	for _, typeParameter := range fact {
		typeParameter, ok := typeParameter.(*types.TypeParameterNode)
		if !ok {
			continue
		}

		instantiated, ok := database.ContainsFact(db, func(node database.Node, fact typecheck.InstantiatedFact) (database.Node, bool) {
			if instantiated, ok := database.GetFact[typecheck.InstantiatedFact](node); ok {
				if instantiated.From == typeParameter && instantiated.Source == source {
					return node, true
				}
			}

			return nil, false
		})

		if !ok {
			continue
		}

		fact, ok := database.GetFact[typecheck.TypedFact](instantiated)
		if !ok || fact.Group == nil {
			continue
		}

		var uses []database.Node
		for _, node := range fact.Group.Nodes {
			if filter(node) {
				if _, ok := database.GetFact[typecheck.TypedFact](node); ok {
					uses = append(uses, node)
				}
			}
		}

		if len(uses) == 0 {
			continue
		}

		// Prefer to use non-hidden nodes if possible
		if slices.ContainsFunc(uses, func(node database.Node) bool {
			return !database.IsHiddenNode(node)
		}) {
			uses = slices.DeleteFunc(uses, database.IsHiddenNode)
		}

		slices.SortStableFunc(uses, func(left database.Node, right database.Node) int {
			return OrderGroupNodes(left) - OrderGroupNodes(right)
		})

		database.SortByProximity(uses, source)

		links[typeParameter.Name] = Link{
			Node:    uses[0],
			Related: uses,
			Types:   fact.Group.Types,
		}
	}

	return links
}
